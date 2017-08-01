package trader

import scala.collection.JavaConverters._
import com.typesafe.scalalogging.LazyLogging
import trader._
import trader.Util._
import scala.util._
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.collection.mutable
import org.knowm.xchange._
import org.knowm.xchange.currency._
import org.knowm.xchange.dto._
import org.knowm.xchange.dto.account._
import org.knowm.xchange.dto.marketdata._
import org.knowm.xchange.dto.meta._
import org.knowm.xchange.dto.trade._
import org.knowm.xchange.service._
import org.knowm.xchange.service.account._
import org.knowm.xchange.service.marketdata._
import org.knowm.xchange.service.trade._
import org.knowm.xchange.service.trade.params._
import org.knowm.xchange.service.trade.params.orders._
import org.knowm.xchange.exceptions._

object Exchanges extends LazyLogging {

  // implicit classes
  implicit class MyExchange(val x: Exchange) {
    def original = x
    override def toString() = x.getExchangeSpecification.getExchangeName
    // override def getAccountService() = super.getAccountService
  }
  implicit def revertExchange(x: MyExchange) = x.original
  // AccountService
  // MarketDataService
  // TradeService

  // types
  type Big = BigDecimal // java.math.BigDecimal
  type Combo = (CurrencyPair, MyExchange)
  type ExcTicker = (MyExchange, Ticker)
  case class BestPairRoutes(askLow: ExcTicker, bidHigh: ExcTicker, ratio: BigDecimal)

  // def wrapNull[A,B](f: (A) => B) = (x: A): B => x match {
  //   case null => null
  //   case _ => f(x)
  // }

  // implicit conversions
  // implicit def s2jBig = wrapNull((x: Big) => x.bigDecimal _)
  // implicit def j2sBig = wrapNull((x: java.math.BigDecimal) => BigDecimal(x))
  implicit def s2jBig(x: Big) = x match {
    case null => null
    case _ => x.bigDecimal
  }
  implicit def j2sBig(x: java.math.BigDecimal) = x match {
    case null => null
    case _ => BigDecimal(x)
  }
  implicit def d2jBig(x: Double) = new java.math.BigDecimal(x)
  implicit def d2sBig(x: Double) = j2sBig(d2jBig(x))

  // constants
  val zero: Big = new java.math.BigDecimal(0.0)
  val one: Big = new java.math.BigDecimal(1.0)
  val maxDiff: Big = new java.math.BigDecimal(10.0)
  val fiat = List("AED", "AFN", "ALL", "AMD", "ANG", "AOA", "ARS", "AUD", "AWG", "AZN", "BAM", "BBD", "BDT", "BGN", "BHD", "BIF", "BMD", "BND", "BOB", "BOV", "BRL", "BSD", "BTN", "BWP", "BYN", "BZD", "CAD", "CDF", "CHE", "CHF", "CHW", "CLF", "CLP", "CNY", "COP", "COU", "CRC", "CUC", "CUP", "CVE", "CZK", "DJF", "DKK", "DOP", "DZD", "EGP", "ERN", "ETB", "EUR", "FJD", "FKP", "GBP", "GEL", "GHS", "GIP", "GMD", "GNF", "GTQ", "GYD", "HKD", "HNL", "HRK", "HTG", "HUF", "IDR", "ILS", "INR", "IQD", "IRR", "ISK", "JMD", "JOD", "JPY", "KES", "KGS", "KHR", "KMF", "KPW", "KRW", "KWD", "KYD", "KZT", "LAK", "LBP", "LKR", "LRD", "LSL", "LYD", "MAD", "MDL", "MGA", "MKD", "MMK", "MNT", "MOP", "MRO", "MUR", "MVR", "MWK", "MXN", "MXV", "MYR", "MZN", "NAD", "NGN", "NIO", "NOK", "NPR", "NZD", "OMR", "PAB", "PEN", "PGK", "PHP", "PKR", "PLN", "PYG", "QAR", "RON", "RSD", "RUB", "RWF", "SAR", "SBD", "SCR", "SDG", "SEK", "SGD", "SHP", "SLL", "SOS", "SRD", "SSP", "STD", "SVC", "SYP", "SZL", "THB", "TJS", "TMT", "TND", "TOP", "TRY", "TTD", "TWD", "TZS", "UAH", "UGX", "USD", "USN", "UYI", "UYU", "UZS", "VEF", "VND", "VUV", "WST", "XAF", "XAG", "XAU", "XBA", "XBB", "XBC", "XBD", "XCD", "XDR", "XOF", "XPD", "XPF", "XPT", "XSU", "XTS", "XUA", "XXX", "YER", "ZAR", "ZMW", "ZWL").map(x => new Currency(x)).toSet
  def isCrypto(coin: Currency): Boolean = !fiat.contains(coin)
  val rounded = new java.math.MathContext(7) // big(rounded)
  val exchangeParams: Map[String, Map[String, Any]] = List(
    "org.knowm.xchange.ripple.RippleExchange" -> List(
      ripple.RippleExchange.PARAMETER_TRUST_API_RIPPLE_COM -> true,
    ),
  ).toMap.mapValues(_.toMap)

  def safeDiv(a: Big, b: Big): Big = if (a == null || b == null || isZero(b)) null else a / b

  def failsExpectation(
    failRule: (List[(Long, Big)], List[Big], My.TimeRule) => Boolean
  )(xys: List[(Long, Big)])(expectation: My.TimeRule) = {
    val now = xys.last._1
    val idx = xys.indexWhere(_._1 < now - (expectation.mins minutes).toMillis)
    idx match {
      case -1 => false // period not yet reached
      case _ =>
        val (_times, prcs): (List[Long], List[Big]) = xys.drop(idx).unzip
        val failed = failRule(xys, prcs, expectation)
        // ^ pair flip agnostic?
        if (failed) {
          // println(s"failed for: ${xys(idx)}")
          println(s"failed for: ${expectation}")
        }
        failed
    }
  }: Boolean

  def fellOver = failsExpectation((xys: List[(Long, Big)], prices: List[Big],
    expectation: My.TimeRule) => prices.last < expectation.rate * prices.head) _
  // TODO: linalg

  def downOver = failsExpectation((xys: List[(Long, Big)],
    prices: List[Big], expectation: My.TimeRule) => {
    val buyPrice = xys.head._2
    prices.forall(_ < expectation.rate * buyPrice)
  }) _

  def makePair(exc: MyExchange, coin: Currency): CurrencyPair = {
    val pairs = exc.getExchangeMetaData.getCurrencyPairs
    val regular = new CurrencyPair(My.baseCoin, coin)
    val reverse = new CurrencyPair(coin, My.baseCoin)
    // Yobit normal, Bittrex/Poloniex flipped
    val pair = if (pairs.containsKey(regular)) regular
          else if (pairs.containsKey(reverse)) reverse
          else throw new Exception(s"pairs for exchange ${exc} is missing a pair for ${coin.getCurrencyCode}!")
    pair
  }

  def pingProblem(signal: Double, tickerOption: Option[Ticker]): Option[String] = Option(tickerOption match {
    case None =>
      // no ticker for BID reference, so market order
      // s"no ticker for ${exc}, ignore"
      null
      // TODO: verify ping by % up
    case Some(ticker) =>
      // println(s"ticker: $ticker")
      val price = ticker.getAsk
      val ratio = safeDiv(price, ticker.getBid)
      println(s"ratio: $ratio")
      println(s"buyMin: ${My.conf.buyMin}")
      val minBuy = signal * My.conf.buyMin
      // println(s"minBuy: ${minBuy}")
      if (price < minBuy) {
        s"price dropped from $signal to $price (minBuy ${minBuy}), ignore"
      } else if (ticker.getVolume < My.conf.minVolume) {
        s"volume ${ticker.getVolume} too low, ignore"
      } else if (ratio > My.conf.allowedGap) {
        s"bid/ask ratio too big (${ratio}), ignore"
      } else null
  })

  def calcBuyAmnt(market: MarketDataService, pair: CurrencyPair, spendAmnt: Big): Big = {
    val orderBook: OrderBook = market.getOrderBook(pair)
    val orders: List[LimitOrder] = (if (isFlipped(pair))
      orderBook.getAsks else orderBook.getBids).asScala.toList
    // v adjust for pair order?
    val (_left, buyAmnt) = orders.foldLeft((spendAmnt, zero))((tpl: (Big, Big), order: LimitOrder) => {
      val (btcLeft, amntAcc) = tpl
      isZero(btcLeft) match {
        case true => (btcLeft, amntAcc) // shortcut
        case _ =>
          val (price, volBase, volCounter) = orderStats(order)
          // val price = order.getLimitPrice // counter/base
          // val volBase = order.getTradableAmount
          // val volCounter = price * volBase
          val spendBtc = btcLeft.min(volCounter)
          // println(s"spendBtc: $spendBtc")
          val coinsBuy = safeDiv(spendBtc, price)
          // println(s"coinsBuy: $coinsBuy")
          (btcLeft - spendBtc, amntAcc + coinsBuy)
      }
    })
    buyAmnt
  }

  def isZero(big: Big): Boolean = big == zero
  // def isZero(big: Big): Boolean = big.doubleValue == 0.0

  def move(fromExc: MyExchange, toExc: MyExchange, amnt: Big, coin: Currency = My.baseCoin) = {
    if (!isCrypto(coin)) {
      throw new Exception("only crypto currencies can be easily moved!")
    }
    val fromAcc = fromExc.getAccountService
    println(s"fromAcc: $fromAcc")
    val   toAcc =   toExc.getAccountService
    println(s"toAcc: $toAcc") // yobit: fails
    val addr = toAcc.requestDepositAddress(coin)
    if (addr != null && addr != "") { // don't need this, right?
      val res = fromAcc.withdrawFunds(coin, amnt, addr)
      println(s"$res")
      // "given the expected withdrawal fee of $fee, it should get $left..."
    } else {
      throw new Exception("empty address!")
    }
  }

  def isFlipped(pair: CurrencyPair): Boolean = My.baseCoin match {
    case pair.base => false
    case pair.counter => true
    case _ => throw new Exception(s"trade pair ${pair} does not include the storage coin")
  }

  def getAlt(pair: CurrencyPair): Currency = if (isFlipped(pair)) pair.base else pair.counter

  def orderKind(isBuy: Boolean, isFlipped: Boolean): Order.OrderType = if (isBuy == isFlipped) Order.OrderType.BID else Order.OrderType.ASK

  // calculate the expected costs (BID) / proceedings (ASK) for a trade
  def calcTotal(trade: UserTrade, exc: MyExchange): Big = {
    val pair = trade.getCurrencyPair
    val feeCoin = trade.getFeeCurrency
    val feeMult: Big = feeCoin match {
      case pair.counter => 1.0
      case _ =>
        // get rate `pair.counter / trade.getFeeCurrency` at exc
        getTicker(exc, new CurrencyPair(feeCoin, pair.counter)) match {
          case Failure(e) => {
            println(s"no ticker for ${exc} to calc fees")
            // TODO: use rate from another exchange
            1.0 // dummy
          }
          case Success(ticker) => {
            // println(s"ticker: $ticker")
            ticker.getAsk // guess pessimistic
          }
        }
    }
    // trade.getTradableAmount // base
    // * trade.getPrice // counter/base
    // - trade.getFeeAmount * feeMult
    trade.getTradableAmount * trade.getPrice - trade.getFeeAmount * feeMult
  }

  def makeExchange(tpl: (String, My.ApiCredential)): List[(String, MyExchange)] = {
    val (name: String, cred: My.ApiCredential) = tpl
    println(s"name: $name")
    // println(s"cred: $cred")
    val spec = new ExchangeSpecification(name)
    spec.setApiKey(cred.apikey)
    spec.setSecretKey(cred.secret)
    // println(s"spec: $spec")
    // spec.setUserName(cred.user)
    // spec.setPassword(cred.pw)
    exchangeParams.applyOrElse(name, (k: String) => Nil).foreach(tpl => {
      val (k, v): (String, Any) = tpl
      spec.setExchangeSpecificParametersItem(k, v)
    })

    retry(3)({
      ExchangeFactory.INSTANCE.createExchange(spec)
    }).map((exc: Exchange) => {
      // println(s"exchange: ${exc}")
      (name, MyExchange(exc))
    }).toOption.toList
  }

  def getTicker(exc: MyExchange, pair: CurrencyPair): Try[Ticker] =
    retry(3)({
      Try(exc.getMarketDataService.getTicker(pair)) // yobit
      .map(timedTicker)
    }).flatMap(identity)

  def timedTicker(t: Ticker): Ticker = t.getTimestamp match {
    case null => tickerMaker(t)
    .timestamp(new java.util.Date(System.currentTimeMillis))
    .build
    case _ => t
  }

  def tickerMaker(t: Ticker): Ticker.Builder = {
    new Ticker.Builder()
    .ask(t.getAsk)
    .bid(t.getBid)
    .currencyPair(t.getCurrencyPair)
    .high(t.getHigh)
    .last(t.getLast)
    .low(t.getLow)
    .volume(t.getVolume)
    .vwap(t.getVwap)
    .timestamp(t.getTimestamp match {
      case null => new java.util.Date(System.currentTimeMillis)
      case ts => ts
    })
  }

  def invertPair(pair: CurrencyPair) = new CurrencyPair(pair.counter, pair.base)

  def invertTicker(t: Ticker): Ticker = {
    new Ticker.Builder()
    // same
    .timestamp(t.getTimestamp)
    .volume(t.getVolume)
    // flipped
    .currencyPair(invertPair(t.getCurrencyPair))
    .vwap(flipNum(t.getVwap))
    .last(flipNum(t.getLast))
    // also metric flipped
    .ask(flipNum(t.getBid))
    .bid(flipNum(t.getAsk))
    .high(flipNum(t.getLow))
    .low(flipNum(t.getHigh))
    .build
  }

  def flipNum(n: Big): Big = n match {
    case null => null
    case big if isZero(big) => null
    case _ => one / n
  }

  def accInfo(exc: MyExchange): Try[AccountInfo] = {
    exc.getAccountService match {
      case null => Failure(new Exception(s"no AccountService for ${exc}"))
      // ^ Yobit
      case ser: AccountService => retry(3)({
        // println(s"ser: $ser")
        // v this may fail
        val acc = ser.getAccountInfo
        // println(s"acc: $acc")
        acc
      })
    }
  }

  def getFailed(tickers: List[Ticker]): List[String] = {
    val xys: List[(Long, Big)] = tickers
    .map((t: Ticker) => (t.getTimestamp.getTime, new Big(t.getAsk)))
    // ^ what about checking BID?
    // println(s"xys: $xys")
    val (times, prices): (List[Long], List[Big]) = xys.unzip
    // sell rules:
    val max = prices.max
    // - if lost n% compared to max since buy
    val maxDrop = prices.last < My.conf.maxDrop * max
    // - if down n% from buy
    val sellBelow = prices.last < My.conf.sellBelow * prices.head
    // - if up n% from buy
    val sellAbove = prices.last > My.conf.sellAbove * prices.head
    // - always sell after a certain number of minutes
    val sellAfter = times.last > (My.conf.sellAfter minutes).toMillis + times.head
    // bid >+n% of buy price then down from max by n%: sell
    val dropPos = 1 + My.conf.maxDropPositive
    val maxDropPositive = max > dropPos && dropPos * prices.last < max
    // - linear regression: if <+1%/10min
    val fallRules = My.conf.fallRules.exists(fellOver(xys))
    // sell when <-x% of buy price over y minutes
    val downRules = My.conf.downRules.exists(downOver(xys))
    val checks = List(
      "maxDrop" -> maxDrop,
      "sellBelow" -> sellBelow,
      "sellAbove" -> sellAbove,
      "sellAfter" -> sellAfter,
      "maxDropPositive" -> maxDropPositive,
      "fallRules" -> fallRules,
      "downRules" -> downRules,
    ).toMap
    checks.filter((tpl2: (String, Boolean)) => tpl2._2).keys.toList
  }

  def getExcTickerOrderBook(tpl: ExcTicker): OrderBook = {
    val (exc, ticker) = tpl
    val market = exc.getMarketDataService
    val pair = ticker.getCurrencyPair
    // ^ ensure this won't flip?
    val orderBook: OrderBook = market.getOrderBook(pair)
    orderBook
  }

  def orderStats(order: LimitOrder): (Big, Big, Big) = {
    val price = order.getLimitPrice // counter/base
    val volBase = order.getTradableAmount
    val volCounter = price * volBase
    (price, volBase, volCounter)
  }

}

class Exchanges(var whitelist: List[String] = Nil) extends LazyLogging {
  import Exchanges._

  // state

  // abstract state into monad to use lenses over mutation?

  val exchanges: Map[String, MyExchange] = My.conf.exchanges
    .filterKeys((k: String) => if (whitelist.isEmpty) true else whitelist.contains(k))
    .toList
    // .map(log)
    .map(x => {
      println(s"x: $x")
      x
    })
    .flatMap(makeExchange)
    .toMap

  // val coins = mutable.Map.empty[Combo, List[Ticker]]
  // ^ also store max?
  val coins: mutable.Map[Combo, List[Ticker]] = mutable.Map(
    exchanges.values.toList
    // .par // makes it not resolve...
    .flatMap((exc: MyExchange) => {
      logger.info(s"LOGGER checking coins for ${exc}...")
      println(s"checking coins for ${exc}...")
      // val pairs = exc.getExchangeMetaData.getCurrencyPairs
      accInfo(exc).map(_.getWallet.getBalances.asScala
        .filter((tpl) => {
          val (coin, bal): (Currency, Balance) = tpl
          val have: Big = bal.getAvailable + bal.getDepositing
          val keep: Boolean = coin != My.baseCoin && !isZero(have)
          keep
        })
        .keys
        .filter((coin: Currency) => isCrypto(coin) && coin != My.baseCoin)
        .map((coin: Currency) => {
          val pair = makePair(exc, coin)
          val tickers = getTicker(exc, pair).toOption.toList
          ((pair, exc), tickers)
        })
      ).getOrElse(Nil).toList
    }: List[(Combo, List[Ticker])]).toMap
  .toSeq: _*)

  // val watchedOrders = Map.empty[MyExchange, mutable.Set[String]]
  val watchedOrders = exchanges.values
  // .par // makes it not resolve...
  .map((exc: MyExchange) => {
    val trade: TradeService = exc.getTradeService
    // println(s"trade: $trade")
    val mut = trade match {
      case null =>
        println(s"failed to get TradeService for ${exc}")
        mutable.Set.empty[String]
      case _ =>
        val params = trade.createOpenOrdersParams
        // println(s"params: $params")
        retry(3)({
          trade.getOpenOrders(params).getOpenOrders.asScala //.toList
        }) match {
          case Success(openOrders) =>
            // ^ ignores any orders filled since script last ran
            // println(s"openOrders: $openOrders") // no open orders
            val ids = openOrders.map(_.getId).toSet
            mutable.Set(ids.toSeq:_*)
          case _ => mutable.Set.empty[String]
        }
    }
    (exc, mut)
  }).toMap

  // use <btcVal> to buy <coin> in <val>
  def buy(pair: CurrencyPair) = transact(orderKind(true, isFlipped(pair)), pair) _

  // sell all <coin> in <pair>
  def sell(pair: CurrencyPair) = transact(orderKind(false, isFlipped(pair)), pair) _

  def transact(orderType: Order.OrderType, pair: CurrencyPair)(exc: MyExchange, tradable: Big, limit: Big): Unit = {
    // println(s"tradable: $tradable")
    // println(s"limit: $limit")
    val acc = exc.getAccountService
    val pairMeta = exc.getExchangeMetaData.getCurrencyPairs.get(pair)
    // CREDIT/BTC=CurrencyPairMetaData [tradingFee=0.2, minimumAmount=0.00010, maximumAmount=null, priceScale=8]
    val trade: TradeService = exc.getTradeService
    // println(s"trade: $trade")
    val hasLimit = !isZero(limit)
    // retryInc { // TODO: ensure a failing action after creation can't make new trade
      // try {
        val orderId: String = if (hasLimit) {
          val order = new LimitOrder.Builder(orderType, pair)
            .limitPrice(limit)
            .tradableAmount(tradable).build
          // println(s"limit order: $order")
          println(s"$orderType $pair $tradable ($exc) @ limit ${limit(rounded)}")
          trade.placeLimitOrder(order)
        } else {
          val order = new MarketOrder.Builder(orderType, pair)
            .tradableAmount(tradable).build
          // println(s"market order: $order")
          println(s"$orderType $pair $tradable ($exc) @ market rate")
          trade.placeMarketOrder(order)
        }
        println(s"orderId: $orderId")
        watchedOrders(exc) += orderId
      // } catch {
      //   case ex: ExchangeException =>
      //     println(ex)
      // }
    // }
  }

  def arb(): Unit = {
    println(s"exchanges: $exchanges")
    // val excPairTickers: Map[MyExchange, Map[CurrencyPair, Ticker]]
    val excPairTickers: List[(MyExchange, List[(CurrencyPair, Ticker)])]
    = exchanges.values.toList.map((exc: MyExchange) => {
      println(s"exc: $exc")
      val pairs = try {
        exc.getExchangeSymbols.asScala.toList
      } catch {
        case ex: java.lang.NullPointerException => {
          println(s"failed to get pairs for exchange ${exc}!")
          Nil
        }
      }
      println(s"pairs: $pairs")
      val market = exc.getMarketDataService
      println(s"market: $market")
      val pairTickers: List[(CurrencyPair, Ticker)] = pairs.flatMap(tryMap((pair: CurrencyPair) => {
        // println(s"pair: $pair")
        retry(3)({
          Try(market.getTicker(pair))
        })
        .flatMap(identity)
        .map(timedTicker)
        .filter((t: Ticker) => t.getAsk != null && t.getAsk > 0 && t.getBid != null && t.getBid > 0)
        .map((ticker: Ticker) => (pair, ticker))
      }))
      println(s"pairTickers: $pairTickers")
      val inverted: List[(CurrencyPair, Ticker)] = pairTickers.map(tpl => {
        val (pair, ticker): (CurrencyPair, Ticker) = tpl
        println(s"ticker: $ticker")
        val invPair = invertPair(pair)
        println(s"invPair: $invPair")
        val invTicker = invertTicker(ticker)
        println(s"invTicker: $invTicker")
        (invPair, invTicker)
      })
      println(s"inverted: $inverted")
      (
        exc,
        pairTickers.++(inverted).toMap.toList
      )
    })
    // .toMap
    println(s"excPairTickers: $excPairTickers")

    // turn inside-out to have pairs on top
    val pairExcTickers: List[(CurrencyPair, ExcTicker)]
    = excPairTickers.flatMap(both => {
      val (exc, tpls) /*: (MyExchange, List[(CurrencyPair, Ticker)])*/ = both
      tpls.map((tpl) => {
        val (pair, ticker) = tpl
        (pair, (exc, ticker))
      })
    })
    println(s"pairExcTickers: $pairExcTickers")

    val pairExcTickerMap: Map[CurrencyPair, List[ExcTicker]]
    = pairExcTickers.groupBy(_._1).map((kv) => {
      val (k,v) = kv
      (k, v.map(_._2))
    })
    println(s"pairExcTickerMap: $pairExcTickerMap")

    val pairCounts: Map[CurrencyPair, Int] = pairExcTickerMap.mapValues(_.size)
    println(s"pairCounts: $pairCounts")

    // filter to asc pairs?
    // for each pair find highest bid, lowest ask
    // - ^ how to deal with amounts?
    //   - calc for fixed amount?
    //   - for higher amounts merge order-books?

    val bestRoutesPairs: Map[CurrencyPair, BestPairRoutes] = pairExcTickerMap
    .filter(_._2.size > 1)
    .flatMap((tpl) => {
      println(s"tpl: $tpl")
      val (pair, tpls): (CurrencyPair, List[ExcTicker]) = tpl
      println(s"pair: $pair")
      println(s"tpls: $tpls")
      val lowestAsk = tpls.minBy(_._2.getAsk)
      println(s"lowestAsk: $lowestAsk")
      val highestBid = tpls.maxBy(_._2.getBid)
      println(s"highestBid: $highestBid")
      (lowestAsk == highestBid match {
        case true => None
        case false => {
          val ratio = safeDiv(highestBid._2.getBid, lowestAsk._2.getAsk)
          println(s"ratio: $ratio")
          val routes = new BestPairRoutes(lowestAsk, highestBid, ratio)
          println(s"routes: $routes")
          Some((pair, routes))
        }
      }).toList
    })
    .filter(_._2.ratio > one)
    .filter(tpl => {
      val (pair, routes) = tpl
      val tooDifferent = routes.ratio > maxDiff
      if (tooDifferent) {
        println(s"$pair ratio fishy between ${routes.askLow._1} and ${routes.bidHigh._1}, ignore: ${routes.ratio}")
      }
      !tooDifferent
    })
    println(s"bestRoutesPairs: $bestRoutesPairs")

    // sort by ratio to find best pair-wise arbitrage
    val sortedPairwiseOptions = bestRoutesPairs
    // .filterKeys((pair: CurrencyPair) => pair.base == Currency.BTC && isCrypto(pair.counter))
    // ^ ignore fiat / non-BTC pairs, flipped dupes
    .toList.sortBy(tpl => {
      val (pair, routes): (CurrencyPair, BestPairRoutes) = tpl
      routes.ratio
    }).reverse
    println(s"sortedPairwiseOptions: ${sortedPairwiseOptions}")

    sortedPairwiseOptions.foreach(tryMap((opt) => Try {
      val (pair, best) = opt
      println(s"opt: $pair ${best.ratio} (${best.askLow._1} -> ${best.bidHigh._1})")
      val maxBid: Big = best.bidHigh._2.getBid
      println(s"maxBid: ${maxBid}")
      val minAsk: Big = best.askLow._2.getAsk
      println(s"minAsk: ${minAsk}")

      val askOrders: List[LimitOrder] = retry(3)({
        getExcTickerOrderBook(best.askLow).getAsks.asScala
        .toList.filter(maxBid < _.getLimitPrice)
      }).toOption.toList.flatMap(identity)
      println(s"askOrders: ${askOrders}")
      val bidOrders: List[LimitOrder] = retry(3)({
        getExcTickerOrderBook(best.bidHigh).getBids.asScala
        .toList.filter(minAsk > _.getLimitPrice)
      }).toOption.toList.flatMap(identity)
      println(s"bidOrders: ${bidOrders}")
      // ^ both sorted from the middle, i.e. ask asc, bid desc.

      var askIdx = 0
      var bidIdx = 0
      var askVolBase    : Big = new java.math.BigDecimal(0.0)
      var bidVolBase    : Big = new java.math.BigDecimal(0.0)
      var bidVolCounter : Big = new java.math.BigDecimal(0.0)
      var askVolCounter : Big = new java.math.BigDecimal(0.0)
      var askPrice = minAsk
      var bidPrice = maxBid
      var crossPrice: Big = new java.math.BigDecimal(0.0)

      // until a price closes the gap...
      while (askPrice < bidPrice &&
            askOrders.isDefinedAt(askIdx) &&
            bidOrders.isDefinedAt(bidIdx)) {
        println(s"looping...")
        println(s"askPrice: ${askPrice}")
        println(s"bidPrice: ${bidPrice}")
        println(s"askVolBase: ${askVolBase}")
        println(s"bidVolBase: ${bidVolBase}")
        // pop an order for side with least volBase
        if (askVolBase <= bidVolBase) {
          val order = askOrders(askIdx) // fails if no more volume?
          println(s"order: ${order}")
          askIdx += 1
          println(s"askIdx: ${askIdx}")
          val (price, volBase, volCounter) = orderStats(order)
          askPrice = price
          println(s"askPrice: ${askPrice}")
          if (askPrice < bidPrice) {
            askVolBase = askVolBase + volBase // no += :(
            println(s"askVolBase: ${askVolBase}")
            askVolCounter = askVolCounter + volCounter
            println(s"askVolCounter: ${askVolCounter}")
          } else {
            val diffVolBase = bidVolBase - askVolBase
            println(s"diffVolBase: ${diffVolBase}")
            val price: Big = Try({
              j2sBig(bidOrders(bidIdx - 1).getLimitPrice)
            }).getOrElse(zero)
            println(s"price: ${price}")
            bidVolCounter = bidVolCounter - diffVolBase * price
            println(s"bidVolCounter: ${bidVolCounter}")
            bidVolBase = askVolBase
            println(s"bidVolBase: ${bidVolBase}")
            crossPrice = bidPrice
            println(s"crossPrice: ${crossPrice}")
          }
        } else {
          val order = bidOrders(bidIdx) // fails if no more volume?
          println(s"order: ${order}")
          bidIdx += 1
          println(s"bidIdx: ${bidIdx}")
          val (price, volBase, volCounter) = orderStats(order)
          bidPrice = price
          println(s"bidPrice: ${bidPrice}")
          if (askPrice < bidPrice) {
            bidVolBase = bidVolBase + volBase // no += :(
            println(s"bidVolBase: ${bidVolBase}")
            bidVolCounter = bidVolCounter + volCounter
            println(s"bidVolCounter: ${bidVolCounter}")
          } else {
            val diffVolBase = askVolBase - bidVolBase
            println(s"diffVolBase: ${diffVolBase}")
            val price: Big = Try({
              j2sBig(askOrders(askIdx - 1).getLimitPrice)
            }).getOrElse(zero)
            println(s"price: ${price}")
            askVolCounter = askVolCounter - diffVolBase * price
            println(s"askVolCounter: ${askVolCounter}")
            askVolBase = bidVolBase
            println(s"askVolBase: ${askVolBase}")
            crossPrice = askPrice
            println(s"crossPrice: ${crossPrice}")
          }
        }

      }

      val volBase = askVolBase
      println(s"volBase: $volBase")
      if (!isZero(volBase)) {
        println(s"askPrice: $askPrice")
        println(s"bidPrice: $bidPrice")
        println(s"crossPrice: $crossPrice")
        val avgBid = safeDiv(bidVolCounter, volBase)
        val avgAsk = safeDiv(askVolCounter, volBase)
        val avgDif = avgAsk - avgBid
        println(s"avgDif: $avgDif")
        val diff = askVolCounter - bidVolCounter
        println(s"diff: $diff")
        println(s"ARB $pair (${best.askLow._1} -> ${best.bidHigh._1}): trade $volBase at avgDif $avgDif for $diff profit!")

        // actually consider:
        // - trade/withdrawal fees
        // - coins (ignore fiat?)
        // - getting assets in right coin/exchange
        //   - insist on BTC as entry coin?
        //   - otherwise calculate shortest path ratio from any have to any want?
        // - risk transfer time?
        // - minimum viable opportunity?
        // then execute
      }
    }))

    // TODO: trade
    val usedExchanges = excPairTickers.filter(_._2.nonEmpty).map(_._1)
    println(s"usedExchanges: ${usedExchanges}")
    val numExchanges: Int = usedExchanges.size
    println(s"numExchanges: ${numExchanges}")

    println(s"done, terminating!")
    system.terminate

  }

  def checkCoin(tpl: (Combo, List[Ticker])): Unit = {
    val ((pair, exc), tckrs) = tpl
    val tickers: List[Ticker] = tckrs ::: getTicker(exc, pair).toOption.toList
    // println(s"tickers: $tickers")
    coins += (((pair, exc), tickers))
    val coin = getAlt(pair)
    val failedChecks = getFailed(tickers)
    if (failedChecks.nonEmpty) {
      val reasons = failedChecks.mkString(", ")
      println(s"SELL ${exc} ${coin}: ${reasons}")
      println(s"buy info: ${tickers.head}")
      println(s"sell info: ${tickers.last}")
      // println(s"prices: ${prices.map(_.toDouble)}")
      val have: Big = accInfo(exc).map(_.getWallet.getBalance(coin).getAvailable) match {
        case Success(squat) if squat.doubleValue == 0.0 =>
          println(s"zero balance... stop monitoring currency?")
          new java.math.BigDecimal(99999)
        case Success(bal) => bal
        case Failure(ex) => new java.math.BigDecimal(99999)
        // ^ don't know how much we have, try and sell everything
      }
      println(s"have: $have")
      // val ticker = getTicker(exc, pair)
      val minSell = getTicker(exc, pair) match {
        case Failure(e) =>
          // println(s"no ticker for ${exc}, selling as market order")
          // TODO: use signal as ASK reference to make limit order
          zero
        case Success(ticker) =>
          // println(s"ticker: $ticker")
          ticker.getAsk * My.conf.sellMin
      }
      try {
        sell(pair)(exc, have, minSell)
        coins -= ((pair, exc))
        // TODO: redistribute across exchanges?
        // println(acc.withdrawFunds(My.baseCoin, btcVal, My.storageAddr))
      } catch {
        case ex: ExchangeException =>
          println(ex)
          // stop watching for permanent errors
          ex.getMessage match {
            case "DUST_TRADE_DISALLOWED_MIN_VALUE_50K_SAT" =>
              coins -= ((pair, exc))
          }
      }
    } else {
      println(s"keep ${exc} ${coin}")
      logger.info(s"LOGGER keep")
    }
  }

  def watchOrders(tpl: (MyExchange, mutable.Set[String])): Unit = {
    val (exc, orderIds) = tpl
    // println(s"watchedOrders(${exc}): ${watchedOrders(exc)}")
    val trade: TradeService = exc.getTradeService
    if (!orderIds.isEmpty && trade != null) {
      try {
        val orderColl: java.util.Collection[Order] =
          trade.getOrder(orderIds.toSeq:_*)
        val orders = orderColl.toArray.toList.asInstanceOf[List[Order]]
        println(s"orders: $orders")
        orders.par.foreach((order: Order) => {
          println(s"status: ${order.getStatus}")
          order.getStatus match {
            case Order.OrderStatus.FILLED => handleFilledOrder(order, exc)
            case status => handleUnfilled(order, exc)
          }
        })
      } catch {
        case not: NotYetImplementedForExchangeException =>
          // println(s"can't get orders for ${exc}")
          // ^ Bittrex... need a way to throw this no more than once

          // check open orders
          retry(3)({
            trade.getOpenOrders(trade.createOpenOrdersParams)
          }).map((openOrders: OpenOrders) => {
            val orders: List[LimitOrder] = openOrders.getOpenOrders.asScala.toList
            println(s"orders: $orders")
            val askedOrders = orders.filter((order: LimitOrder) => orderIds.contains(order.getId))
            println(s"askedOrders: $askedOrders")
            askedOrders.par.foreach((order: LimitOrder) => {
              println(s"status: ${order.getStatus}")
              handleUnfilled(order, exc)
            })
          })

          // check completed trades
          retry(3)({
            trade.getTradeHistory(trade.createTradeHistoryParams)
          }).map((userTrades: UserTrades) => {
            val trades: List[UserTrade] = userTrades.getUserTrades.asScala.toList
            // println(s"trades: $trades")
            val askedTrades = trades
              .filter((trade: UserTrade) => orderIds.contains(trade.getOrderId))
            println(s"askedTrades: $askedTrades")
            askedTrades.par.foreach((trade: UserTrade) => {
              handleFilledTrade(trade, exc)
            })
          })

      }
    }
  }

  def handleUnfilled(order: Order, exc: MyExchange): Unit = {
    println(s"unfilled: $order")
    // println(s"status: $status")
    // // TODO: cancel if the market moved and we no longer expect it to fill / be profitable
    // val canceled: boolean = trade.cancelOrder(order.id)
    // println(s"canceled: $canceled")
  }

  def invest(exc: MyExchange, coin: Currency, signal: Double): Unit = {
    println(s"PING $coin $exc $signal")
    val pair = makePair(exc, coin)
    // val ticker = getTicker(exc, pair)
    val tickerOption = getTicker(exc, pair).toOption
    pingProblem(signal, tickerOption) match {
      case Some(e) => println(e)
      case None =>
        val limit = tickerOption.map((ticker) => ticker.getAsk * My.conf.buyMax).getOrElse(zero)
        // tickerOption.foreach((ticker) => println(s"ticker: $ticker"))
        // ^ zero makes a market order if no BID info
        println(s"BUY (signal ${signal}, limit ${limit.doubleValue})")
        val need = My.conf.investment
        val btcVal: Big = accInfo(exc).map(_.getWallet
          .getBalance(Currency.BTC).getAvailable) match {
          case Success(haveBTC) =>
            println(s"haveBTC: $haveBTC")
            haveBTC.min(new java.math.BigDecimal(need))
            // if (need > haveBTC) // need more, move
            // move(fromExc, exc, need - haveBTC, My.baseCoin)]
            // TODO: return `need` only when the money is in...
          case Failure(ex) =>
            println(ex)
            need
        }
        val market = exc.getMarketDataService
        val buyAmnt = if (isFlipped(pair))
          calcBuyAmnt(market, pair, btcVal)
          else btcVal
        println(s"buyAmnt: $buyAmnt")
        // retryInc {
          try {
            buy(pair)(exc, buyAmnt, limit)
            val tpl = (pair, exc)
            if (!coins.contains(tpl)) {
              val tickers = tickerOption.toList
              coins += ((tpl, tickers))
              println(s"start to monitor: $tpl")
            }
          } catch {
            case ex: ExchangeException =>
              println(ex)
          }
        // }
    }
  }

  def handleFilledTrade(trade: UserTrade, exc: MyExchange): Unit = {
    println(s"filled trade: $trade")
    val pair = trade.getCurrencyPair
    val total = calcTotal(trade, exc)
    val sign = trade.getType match {
      case Order.OrderType.ASK => "+"
      case Order.OrderType.BID => "-"
      case _ =>
        println(s"unexpected trade type ${trade.getType}")
        "?"
    }
    println(s"expected trade total: ${pair.counter} ${sign}${total}")

    watchedOrders(exc) -= trade.getOrderId
    finishedPair(pair, exc)
  }

  def handleFilledOrder(order: Order, exc: MyExchange): Unit = {
    println(s"filled order: $order")
    val avgPrice = order.getAveragePrice
    println(s"avgPrice: $avgPrice")
    val amount = order.getCumulativeAmount
    println(s"amount: $amount")

    watchedOrders(exc) -= order.getId
    val pair = order.getCurrencyPair
    finishedPair(pair, exc)
  }

    // update coins, maybe move/reinvest assets
  def finishedPair(pair: CurrencyPair, exc: MyExchange): Unit = {
    val coin = getAlt(pair)
    // if I no longer have this coin at this exchange...
    accInfo(exc).map(_.getWallet.getBalance(coin).getAvailable) match {
      case Success(bal) => bal.doubleValue match {
        // no more of this coin, stop checking it
        case 0.0 =>
          // coins -= ((pair, exc))
        case _ =>
          // successful buy?
          // TODO: consider best exchange to sell on. note risk transfer time.
      }
      case Failure(ex) => println(ex)
    }
  }

}
