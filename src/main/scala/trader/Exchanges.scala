package trader

import scala.collection.JavaConverters._
import trader._
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

object Exchanges {

  type Big = java.math.BigDecimal
  type Combo = (CurrencyPair, Exchange)

  // abstract state into monad to use lenses over mutation?

  val exchanges: Map[String, Exchange] =
    My.conf.exchanges.toList.map(makeExchange).toMap

  // val coins = mutable.Map.empty[Combo, List[Ticker]]
  // ^ also store max?
  val coins: mutable.Map[Combo, List[Ticker]] = mutable.Map(
    exchanges.values.toList
    // .par // makes it not resolve...
    .flatMap((exc: Exchange) => {
      // val pairs = exc.getExchangeMetaData.getCurrencyPairs
      accInfo(exc).map(_.getWallet.getBalances.asScala
        .filter((tpl) => {
          val (coin, bal): (Currency, Balance) = tpl
          val have: Big = bal.getAvailable.add(bal.getDepositing)
          !isZero(have)
          // val keep: Boolean = /*coin != My.baseCoin &&*/ !isZero(have)
          // keep
        })
        .map((tpl) => { // tap
          val (coin, bal): (Currency, Balance) = tpl
          println(s"${exc} ${coin}: ${bal}")
          tpl
        })
        .keys
        .filter(_ != My.baseCoin)
        .map((coin: Currency) => {
          val pair = makePair(exc, coin)
          val tickers = getTicker(exc, pair).toList
          ((pair, exc), tickers)
        })
      ).getOrElse(Nil).toList
    }: List[(Combo, List[Ticker])]).toMap
  .toSeq: _*)

  // val watchedOrders = Map.empty[Exchange, mutable.Set[String]]
  val watchedOrders = exchanges.values
  // .par // makes it not resolve...
  .map((exc: Exchange) => {
    val trade: TradeService = exc.getTradeService
    // println(s"trade: $trade")
    val mut = trade match {
      case null => {
        println(s"failed to get TradeService for ${exc}")
        mutable.Set.empty[String]
      }
      case _ => {
        val params = trade.createOpenOrdersParams
        println(s"params: $params")
        val openOrders = Util.retryInc {
          trade.getOpenOrders(params).getOpenOrders.asScala //.toList
        }
        // ^ ignores any orders filled since script last ran
        // println(s"openOrders: $openOrders") // no open orders
        val ids = openOrders.map(_.getId).toSet
        mutable.Set(ids.toSeq:_*)
      }
    }
    (exc, mut)
  }).toMap

  val cancellable = Util.system.scheduler.schedule(0 seconds, 1 minute) {
    coins.par.foreach(checkCoin)
    watchedOrders.toList.par.foreach(watchOrders) // Function.tupled(f)
  }

  def checkCoin(tpl: (Combo, List[Ticker])) = {
    val ((pair, exc), tckrs) = tpl
    val tickers: List[Ticker] = tckrs ::: getTicker(exc, pair).toList
    println(s"tickers: $tickers")
    coins += (((pair, exc), tickers))
    val xys: List[(Long, BigDecimal)] = tickers
    .map((t: Ticker) => (t.getTimestamp.getTime, new BigDecimal(t.getAsk)))
    // ^ what about checking BID?
    // println(s"xys: $xys")
    val (_times, prices): (List[Long], List[BigDecimal]) = xys.unzip
    // sell rules:
    // - below buying point (5s)
    val belowBuy = prices.last < prices.head
    // - if lost n% compared to max since buy (5s)
    val underPeak = prices.last < My.maxDrop * prices.max
    // - linear regression: if <+1%/10min
    val underPerforming = My.timeRules.exists(failsExpectations(xys))
    val doSell = belowBuy || underPeak || underPerforming
    if (doSell) {
      val coin = getAlt(pair)
      println(s"SELL ${exc} ${coin}")
      println(s"prices: ${prices.map(_.toDouble)}")
      println(s"belowBuy: ${belowBuy}")
      println(s"underPeak: ${underPeak}")
      println(s"underPerforming: ${underPerforming}")
      val have: Big = accInfo(exc).map(_.getWallet.getBalance(coin).getAvailable) match {
        case Success(squat) if squat.doubleValue == 0.0 => {
          println(s"zero balance... stop monitoring currency?")
          new Big(99999)
        }
        case Success(bal) => bal
        case Failure(ex) => new Big(99999)
        // ^ don't know how much we have, try and sell everything
      }
      println(s"have: $have")
      // val ticker = getTicker(exc, pair)
      val minSell = getTicker(exc, pair) match {
        case None => {
          println(s"no ticker for ${exc}, selling as market order")
          // TODO: use signal as ASK reference to make limit order
          zero
        }
        case Some(ticker) => {
          println(s"ticker: $ticker")
          new Big(My.sellMin).multiply(ticker.getAsk)
        }
      }
      try {
        sell(pair)(exc, have, minSell)
        coins -= ((pair, exc))
        // TODO: redistribute across exchanges?
        // println(acc.withdrawFunds(My.baseCoin, btcVal, My.storageAddr))
      } catch {
        case ex: ExchangeException => {
          println(s"ex: $ex")
        }
      }
    } else {
      println(s"keep")
    }
  }

  def failsExpectations(xys: List[(Long, BigDecimal)])(expectation: (Double, FiniteDuration)) = {
    val (limit, duration) = expectation
    val now = xys.last._1
    val idx = xys.indexWhere(_._1 < now - duration.toMillis)
    idx match {
      case -1 => false // period not yet reached
      case _ => {
        println(s"expectation: ${expectation}")
        val (_times, prcs): (List[Long], List[BigDecimal]) = xys.drop(idx).unzip
        val failed = prcs.last < limit * prcs.head // TODO: linalg
        // ^ pair flip agnostic?
        if (failed) {
          // println(s"datapoints: ${xys}")
          println(s"failed for: ${xys(idx)}")
        }
        failed
      }
    }
  }: Boolean

  def makePair(exc: Exchange, coin: Currency): CurrencyPair = {
    val pairs = exc.getExchangeMetaData.getCurrencyPairs
    val regular = new CurrencyPair(My.baseCoin, coin)
    val reverse = new CurrencyPair(coin, My.baseCoin)
    // Yobit normal, Bittrex/Poloniex flipped
    val pair = if (pairs.containsKey(regular)) regular
          else if (pairs.containsKey(reverse)) reverse
          else throw new Exception(s"pairs for exchange ${exc.getExchangeSpecification.getExchangeName} is missing a pair for ${coin.getCurrencyCode}!")
    pair
  }

  def pingProblem(signal: Double, tickerOption: Option[Ticker]): Option[String] = Option(tickerOption match {
    case None => {
      // no ticker for BID reference, so market order
      // s"no ticker for ${exc}, ignore"
      null
      // TODO: verify ping by % up
    }
    case Some(ticker) => {
      println(s"ticker: $ticker")
      val price = ticker.getAsk
      val ratio = price.divide(ticker.getBid, 10, java.math.RoundingMode.HALF_EVEN)
      println(s"ratio: $ratio")
      println(s"buyMin: ${My.buyMin}")
      val minBuy = new Big(My.buyMin * signal)
      // println(s"minBuy: ${minBuy}")
      if (price.compareTo(minBuy) < 0) {
        s"price dropped from $signal to $price (minBuy ${minBuy.doubleValue}), ignore"
      } else if (ticker.getVolume.compareTo(new Big(My.minVolume)) < 0) {
        s"volume ${ticker.getVolume} too low, ignore"
      } else if (ratio.compareTo(new Big(My.allowedGap)) > 0) {
        s"bid/ask ratio too big (${ratio}), ignore"
      } else null
    }
  })

  def invest(exc: Exchange, coin: Currency, signal: Double) = {
    println(s"exc: $exc")
    println(s"coin: $coin")
    println(s"signal: $signal")
    val pair = makePair(exc, coin)
    // val ticker = getTicker(exc, pair)
    val tickerOption = getTicker(exc, pair)
    pingProblem(signal, tickerOption) match {
      case Some(e) => println(e)
      case None => {
        val limit = tickerOption.map((ticker) => new Big(My.buyMax).multiply(ticker.getAsk)).getOrElse(zero)
        // tickerOption.foreach((ticker) => println(s"ticker: $ticker"))
        // ^ zero makes a market order if no BID info
        println(s"BUY (signal $signal, limit ${limit.doubleValue})")
        val need = new Big(My.investment)
        val btcVal = accInfo(exc).map(_.getWallet.getBalance(Currency.BTC).getAvailable) match {
          case Success(haveBTC) => {
            println(s"haveBTC: $haveBTC")
            need.min(haveBTC)
            // if (need.compareTo(haveBTC) > 0) // need more, move
            // move(fromExc, exc, need.subtract(haveBTC), My.baseCoin)]
            // TODO: return `need` only when the money is in...
          }
          case Failure(ex) => {
            println(s"ex: $ex")
            need
          }
        }
        val market = exc.getMarketDataService
        Util.retryInc {
          val buyAmnt = if (isFlipped(pair)) calcBuyAmnt(market, pair, btcVal) else btcVal
          println(s"buyAmnt: $buyAmnt")
          try {
            buy(pair)(exc, buyAmnt, limit)
            val tpl = (pair, exc)
            if (!coins.contains(tpl)) {
              val tickers = tickerOption.toList
              coins += ((tpl, tickers))
              println(s"start to monitor: $tpl")
            }
          } catch {
            case ex: ExchangeException => {
              println(s"ex: $ex")
            }
          }
        }
      }
    }
  }

  def calcBuyAmnt(market: MarketDataService, pair: CurrencyPair, spendAmnt: Big): Big = {
    val orderBook: OrderBook = market.getOrderBook(pair)
    val orders: List[LimitOrder] = (if (isFlipped(pair)) orderBook.getAsks else orderBook.getBids).asScala.toList
    // v adjust for pair order?
    val (_left, buyAmnt) = orders.foldLeft((spendAmnt, zero))((tpl: (Big, Big), order: LimitOrder) => {
      val (btcLeft, amntAcc) = tpl
      isZero(btcLeft) match {
        case true => (btcLeft, amntAcc) // shortcut
        case _ => {
          // println(s"btcLeft: $btcLeft")
          // println(s"amntAcc: $amntAcc")
          val price = order.getLimitPrice // coin in BTC
          // println(s"price: $price")
          val availableCoin = order.getTradableAmount
          // println(s"availableCoin: $availableCoin")
          val availableBtc = price.multiply(availableCoin)
          // println(s"availableBtc: $availableBtc")
          val spendBtc = btcLeft.min(availableBtc)
          // println(s"spendBtc: $spendBtc")
          val coinsBuy = spendBtc.divide(price, 10, java.math.RoundingMode.HALF_EVEN)
          // println(s"coinsBuy: $coinsBuy")
          (btcLeft.subtract(spendBtc), amntAcc.add(coinsBuy))
        }
      }
    })
    buyAmnt
  }

  val zero = new Big(0.0)
  def isZero(big: Big): Boolean = big.compareTo(zero) == 0
  // def isZero(big: Big): Boolean = big.doubleValue == 0.0

  def move(fromExc: Exchange, toExc: Exchange, amnt: Big, coin: Currency = My.baseCoin) = {
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

  // use <btcVal> to buy <coin> in <val>
  def buy(pair: CurrencyPair) = transact(orderKind(true, isFlipped(pair)), pair) _

  // sell all <coin> in <pair>
  def sell(pair: CurrencyPair) = transact(orderKind(false, isFlipped(pair)), pair) _

  def isFlipped(pair: CurrencyPair): Boolean = My.baseCoin match {
    case pair.base => false
    case pair.counter => true
    case _ => throw new Exception(s"trade pair ${pair} does not include the storage coin")
  }

  def getAlt(pair: CurrencyPair): Currency = if (isFlipped(pair)) pair.base else pair.counter

  def orderKind(isBuy: Boolean, isFlipped: Boolean): Order.OrderType = if (isBuy == isFlipped) Order.OrderType.BID else Order.OrderType.ASK

  def transact(orderType: Order.OrderType, pair: CurrencyPair)(exc: Exchange, tradable: Big, limit: Big): Unit = {
    println(s"tradable: $tradable")
    println(s"limit: $limit")
    val acc = exc.getAccountService
    val pairMeta = exc.getExchangeMetaData.getCurrencyPairs.get(pair)
    // CREDIT/BTC=CurrencyPairMetaData [tradingFee=0.2, minimumAmount=0.00010, maximumAmount=null, priceScale=8]
    val trade: TradeService = exc.getTradeService
    println(s"trade: $trade")
    val hasLimit = !isZero(limit)
    // Util.retryInc { // TODO: ensure a failing action after creation can't make new trade
      // try {
        val orderId: String = if (hasLimit) {
          val order = new LimitOrder.Builder(orderType, pair)
            .limitPrice(limit)
            .tradableAmount(tradable).build
          println(s"limit order: $order")
          trade.placeLimitOrder(order)
        } else {
          val order = new MarketOrder.Builder(orderType, pair)
            .tradableAmount(tradable).build
          println(s"market order: $order")
          trade.placeMarketOrder(order)
        }
        println(s"orderId: $orderId")
        watchedOrders(exc) += orderId
      // } catch {
      //   case ex: ExchangeException => {
      //     println(s"ex: $ex")
      //   }
      // }
    // }
  }

  def watchOrders(tpl: (Exchange, mutable.Set[String])): Unit = {
    // exc: Exchange, orderId: String
    val (exc, orderIds) = tpl
    val trade: TradeService = exc.getTradeService
    if (!orderIds.isEmpty && trade != null) {
      try {
        val orderColl: java.util.Collection[Order] =
          trade.getOrder(orderIds.toSeq:_*)
          // ^ bittrex: NotYetImplementedForExchangeException
        val orders = orderColl.toArray.toList.asInstanceOf[List[Order]]
        println(s"orders: $orders")
        orders.par.foreach((order: Order) => {
          // getAveragePrice, getCumulativeAmount, getId, getStatus, getTimestamp, getTradableAmount, limitPrice
          // val orderId = order.getId
          println(s"orderId: ${order.getId}")
          println(s"status: ${order.getStatus}")
          order.getStatus match {
            // val orderStatus = OrderStatus.CANCELED // EXPIRED, FILLED, NEW, PARTIALLY_FILLED, PENDING_CANCEL, PENDING_NEW, PENDING_REPLACE, REJECTED, REPLACED, STOPPED
            case Order.OrderStatus.FILLED => handleFilled(order, exc)
            case status => {
              // println(s"status: $status")
              // // TODO: cancel if the market moved and we no longer expect it to fill / be profitable
              // val canceled: boolean = trade.cancelOrder(orderId)
              // println(s"canceled: $canceled")
            }
          }
        })
      } catch {
        case not: NotYetImplementedForExchangeException => {
          // println(s"can't get orders for ${exc}")
          // ^ Bittrex... need a way to throw this no more than once
        }
      }
    }
  }

  def handleFilled(order: Order, exc: Exchange) = {
    val pair = order.getCurrencyPair
    val coin = getAlt(pair)
    val avgPrice = order.getAveragePrice
    println(s"avgPrice: $avgPrice")
    val amount = order.getCumulativeAmount
    println(s"amount: $amount")
    // update coins / watchedOrders, maybe move/reinvest assets
    watchedOrders(exc) -= order.getId
    // if I no longer have this coin at this exchange...
    accInfo(exc).map(_.getWallet.getBalance(coin).getAvailable) match {
      case Success(bal) => new BigDecimal(bal).toDouble match {
        // no more of this coin, stop checking it
        case 0.0 => {
          // val pair = makePair(exc, coin)
          // coins -= ((pair, exc))
        }
        case _ => {
          // successful buy?
          // TODO: consider best exchange to sell on. note risk transfer time.
        }
      }
      case Failure(ex) => println(s"ex: $ex")
    }
  }

  def makeExchange(tpl: (String, My.ApiCredential)) = {
    val (name: String, cred: My.ApiCredential) = tpl
    // println(s"name: $name")
    // println(s"cred: $cred")
    val spec = new ExchangeSpecification(name)
    spec.setApiKey(cred.apikey)
    spec.setSecretKey(cred.secret)
    // println(s"spec: $spec")
    // spec.setUserName(cred.user)
    // spec.setPassword(cred.pw)
    val exc: Exchange = Util.retryInc {
      ExchangeFactory.INSTANCE.createExchange(spec)
    }
    println(s"exchange: $exc")
    (name, exc)
    // val struct = ExchangeStruct(exc, exc.getDefault)
    // (name, struct)
  }

  def getTicker(exc: Exchange, pair: CurrencyPair): Option[Ticker] =
    Util.retryInc {
      Option(exc.getMarketDataService.getTicker(pair)) // yobit
      .map((t: Ticker) => t.getTimestamp match {
        case null => new Ticker.Builder()
        .ask(t.getAsk)
        .bid(t.getBid)
        .currencyPair(t.getCurrencyPair)
        .high(t.getHigh)
        .last(t.getLast)
        .low(t.getLow)
        .volume(t.getVolume)
        .vwap(t.getVwap)
        .timestamp(new java.util.Date(System.currentTimeMillis))
        .build
        case _ => t
      })
    }

  def accInfo(exc: Exchange) = {
    exc.getAccountService match {
      case null => Failure(new Exception(s"no AccountService for ${exc}"))
      // ^ Yobit
      case ser: AccountService => Util.retry(3)({
        // println(s"ser: $ser")
        // v this may fail
        val acc = ser.getAccountInfo
        // println(s"acc: $acc")
        acc
      })
    }
  }: Try[AccountInfo]

  val fiat = List("AED", "AFN", "ALL", "AMD", "ANG", "AOA", "ARS", "AUD", "AWG", "AZN", "BAM", "BBD", "BDT", "BGN", "BHD", "BIF", "BMD", "BND", "BOB", "BOV", "BRL", "BSD", "BTN", "BWP", "BYN", "BZD", "CAD", "CDF", "CHE", "CHF", "CHW", "CLF", "CLP", "CNY", "COP", "COU", "CRC", "CUC", "CUP", "CVE", "CZK", "DJF", "DKK", "DOP", "DZD", "EGP", "ERN", "ETB", "EUR", "FJD", "FKP", "GBP", "GEL", "GHS", "GIP", "GMD", "GNF", "GTQ", "GYD", "HKD", "HNL", "HRK", "HTG", "HUF", "IDR", "ILS", "INR", "IQD", "IRR", "ISK", "JMD", "JOD", "JPY", "KES", "KGS", "KHR", "KMF", "KPW", "KRW", "KWD", "KYD", "KZT", "LAK", "LBP", "LKR", "LRD", "LSL", "LYD", "MAD", "MDL", "MGA", "MKD", "MMK", "MNT", "MOP", "MRO", "MUR", "MVR", "MWK", "MXN", "MXV", "MYR", "MZN", "NAD", "NGN", "NIO", "NOK", "NPR", "NZD", "OMR", "PAB", "PEN", "PGK", "PHP", "PKR", "PLN", "PYG", "QAR", "RON", "RSD", "RUB", "RWF", "SAR", "SBD", "SCR", "SDG", "SEK", "SGD", "SHP", "SLL", "SOS", "SRD", "SSP", "STD", "SVC", "SYP", "SZL", "THB", "TJS", "TMT", "TND", "TOP", "TRY", "TTD", "TWD", "TZS", "UAH", "UGX", "USD", "USN", "UYI", "UYU", "UZS", "VEF", "VND", "VUV", "WST", "XAF", "XAG", "XAU", "XBA", "XBB", "XBC", "XBD", "XCD", "XDR", "XOF", "XPD", "XPF", "XPT", "XSU", "XTS", "XUA", "XXX", "YER", "ZAR", "ZMW", "ZWL").map(x => new Currency(x)).toSet
  def isCrypto(coin: Currency): Boolean = !fiat.contains(coin)

}