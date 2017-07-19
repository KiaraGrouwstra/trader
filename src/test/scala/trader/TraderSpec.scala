package trader

import org.scalatest._
import trader._
import trader.Exchanges._
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

class TraderSpec extends FlatSpec with Matchers {

  val pair: CurrencyPair = new CurrencyPair(Currency.BTC, Currency.ETH)

  val ticker: Ticker = tickerMaker(
  	new Ticker.Builder()
    // .timestamp(...)
    .currencyPair(pair)
    .volume(one)
    .vwap(one)
    .last(one)
    .ask(one)
    .bid(one)
    .high(one)
    .low(one)
    .build
  ).build

  "invertTicker" should "invert a Ticker" in {
  	val ask: Big = invertTicker(ticker).getAsk
    ask shouldEqual one
  }

}
