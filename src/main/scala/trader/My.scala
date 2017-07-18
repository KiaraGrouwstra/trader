package trader

import org.knowm.xchange.currency._
import scala.concurrent.duration._
import cats.syntax.either._
import io.circe.generic.auto._

object My {

  case class CryptoPing(user: String, pw: String)
  case class ApiCredential(apikey: String, secret: String)
  case class TimeRule(mins: Double, rate: Double)
  case class Conf(
    pingInterval: Int, // seconds
    priceInterval: Int, // seconds
    investment: Double, // BTC
    buyMin: Double,
    buyMax: Double,
    sellMin: Double, // make relative to curve?
    allowedGap: Double,
    minVolume: Int, // BTC/mo
    maxDrop: Double,
    maxAge: Int, // minutes
    timeRules: List[TimeRule],
    cryptoping: CryptoPing,
    wallets: Map[String, String], // key: currency code, value: address
    exchanges: Map[String, ApiCredential]) // key: XChange class path

  val conf = Util.yamlFile("conf.yml")
    .flatMap(_.as[Conf])
    .valueOr(throw _)

  val baseCoin = Currency.BTC
  val storageAddr = conf.wallets(baseCoin.getSymbol)
  // TODO: tweak params with ML history
  // TODO: coin/exchange blacklists?
}
