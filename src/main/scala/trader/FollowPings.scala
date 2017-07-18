package trader

import trader._
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

object FollowPings {
  def main(args: Array[String]): Unit = {
    // checkHistory
    val exchanges = new Exchanges(List(
      "org.knowm.xchange.bittrex.v1.BittrexExchange",
      // "org.knowm.xchange.yobit.YoBitExchange",
      // "org.knowm.xchange.poloniex.PoloniexExchange",
    ))
    // println(s"exchanges: ${exchanges.exchanges}")
    // println(s"exchange coins: ${exchanges.coins}")
    val pingCancelable = Util.system.scheduler
    .schedule(0 seconds, My.conf.pingInterval seconds) {
      CpingScraper.scrapeCPing(exchanges)
    }
    val coinCancelable = Util.system.scheduler
    .schedule(0 seconds, My.conf.priceInterval seconds) {
      // println(s"coins: ${exchanges.coins}")
      exchanges.coins.par.foreach(exchanges.checkCoin)
      // println(s"watchedOrders: ${exchanges.watchedOrders}")
      exchanges.watchedOrders.toList.par.foreach(exchanges.watchOrders) // Function.tupled(f)
    }
  }
}
