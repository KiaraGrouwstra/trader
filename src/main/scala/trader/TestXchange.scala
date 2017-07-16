package trader

import scala.collection.JavaConverters._
import trader._
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

object TestXchange {

  // val key =

      // "org.knowm.xchange.poloniex.PoloniexExchange"
      // tradingFee=0.002, minimumAmount=null
      // ok (wallet/{null:wallet}) // fee 0.25% for taker <600BTC/mo
      // issues: ExchangeException

      // "org.knowm.xchange.bittrex.v1.BittrexExchange"
      // account info empty // fee 0.25%, minimumAmount=0.0005 (in BTC)
      // issues: min/max range too tight and can't calculate .. so get
      // either INSUFFICIENT_FUNDS or DUST_TRADE_DISALLOWED_MIN_VALUE_50K_SAT
      // errors: https://support.bittrex.com/hc/en-us/articles/115000240791-Error-Codes-Troubleshooting-common-error-codes

      // "org.knowm.xchange.yobit.YoBitExchange"
      // tradingFee=0.2, minimumAmount=0.0001
      // getAccountService -> null // fee 0.2%, withdrawal fee 0.0001 (BTC?)
      // issues: null for AccountService and Ticker

  def main(args: Array[String]): Unit = {
    // val exc = Exchanges.exchanges(key)
    // println(s"exc: $exc")

    // val pairs = exc.getExchangeMetaData.getCurrencyPairs
    // val altcoins: List[String] = pairs.keys.toList
    //   .filter(_.counter == Currency.BTC)
    //   .map(_.base.getCurrencyCode)
    // println(s"altcoins: $altcoins")

    // // val acc = exc.getAccountService.getAccountInfo
    // val ser = exc.getAccountService
    // println(s"ser: $ser")
    // // null for Yobit
    // if (ser != null) Util.retryInc {
    //   // v this may fail
    //   val acc = ser.getAccountInfo
    //   // println(s"acc: $acc")
    //   // println(List(
    //   //   // "getTradingFee" -> acc.getTradingFee, // BigDecimal
    //   //   // "getUsername" -> acc.getUsername, // String
    //   //   "getWallet" -> acc.getWallet, // Wallet
    //   //   // "getWallets" -> acc.getWallets // Map<String,Wallet>
    //   //   // acc.getWallet(String id) // Wallet
    //   // ).toMap)
    // }

    // val pair = new CurrencyPair(Currency.ETH, Currency.BTC)
    // println(s"pair: $pair")
    // // v non-existing pairs: org.knowm.xchange.exceptions.ExchangeException: INVALID_MARKET
    // Util.retryInc {
    //   val market = exc.getMarketDataService
    //   println(s"market: $market")
    //   val orderBook: OrderBook = market.getOrderBook(pair)
    //   println(s"orderBook: $orderBook")
    //   // [timestamp, asks/bids: each [LimitOrder [limitPrice, Order [type, tradableAmount, currencyPair, null: averagePrice/id/timestamp/status]]
    //   val ticker: Ticker = market.getTicker(pair)
    //   // println(s"ticker: $ticker")
    //   // Ticker [last, bid, ask, high, low, volume, currencyPair, avg=null, timestamp=null]
    //   val trades: Trades = market.getTrades(pair)
    //   // println(s"trades: $trades")
    //   // each [type, tradableAmount, currencyPair, price, timestamp, id]
    // }

  //  val poloniex = Exchanges.exchanges("org.knowm.xchange.poloniex.PoloniexExchange")
  //  val bittrex = Exchanges.exchanges("org.knowm.xchange.bittrex.v1.BittrexExchange")
  //  val yobit = Exchanges.exchanges("org.knowm.xchange.yobit.YoBitExchange")

	// val res = Exchanges.move(
	//   poloniex,
	//   bittrex,
	//   new java.math.BigDecimal(0.0009)
	// )
	// // ^ yobit fails
  // println(s"res: ${res}")

	// val ex = poloniex
	// Exchanges.invest(ex, new Currency("BCY"), 0.00019900)

	// val ex = bittrex
	// Exchanges.invest(ex, new Currency("PKB"), 0.00011500)

  Exchanges.arb

  }

}
