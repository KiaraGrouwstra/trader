package trader

import org.knowm.xchange.currency._
import scala.concurrent.duration._
import cats.syntax.either._
import io.circe.generic.auto._

object My {

  case class CryptoPing(user: String, pw: String)
  case class ApiCredential(apikey: String, secret: String)
  case class Conf(
    cryptoping: CryptoPing,
    wallets: Map[String, String], // key: currency code, value: address
    exchanges: Map[String, ApiCredential]) // key: XChange class path

  val conf = Util.yamlFile("conf.yml")
    .flatMap(_.as[Conf])
    .valueOr(throw _)

  val baseCoin = Currency.BTC
  val storageAddr = conf.wallets(baseCoin.getSymbol)
  // v TODO: tweak params with ML history
  val fetchInterval = 1 minute
  val investment = 0.000545 // 0.0006 // BTC, Bittrex minimum // -> $1.24 // 1.0
  val buyMin = 0.99995 // skip buy if price < this * ping val
  val buyMax = 1.005 // max price willing to buy at (relative from cheapest)
  val sellMin = 0.98 // min price willing to sell at (relative from highest) // make relative to curve?
  val allowedGap = 1.05 // 1.03 // max allowed bid/ask spread
  val minVolume = 10000 // skip altcoins under min trade volume (BTC/mo)
  val maxDrop = 0.99 // sell if down this much from max since buy
  val maxAge = 5 minutes
  // val maxPings = 5
  // val maxAge = 15 hours
  val maxPings = 1
  // sell if the prices doesn't live up to expectations over different time periods
  val timeRules: List[Tuple2[Double, FiniteDuration]] = List(
    (0.995, 10 seconds),
    (0.999, 1 minutes),
    (1.0, 3 minutes),
    (1.01, 10 minutes),
    (1.02, 15 minutes),
  )
  // TODO: coin/exchange blacklists?
}
