package trader

import trader._
// import java.util._
import java.util.{Date,Calendar,TimeZone}
import com.typesafe.scalalogging.LazyLogging
//import monocle._
//import monocle.macros._
import monocle.macros.syntax.lens._
import shapeless._
import syntax.std.tuple._
import syntax.std.traversable._
import syntax.std.maps._
import net.ruippeixotog.scalascraper.browser.JsoupBrowser._
import net.ruippeixotog.scalascraper.browser._
import net.ruippeixotog.scalascraper.dsl._
import net.ruippeixotog.scalascraper.dsl.DSL._
import net.ruippeixotog.scalascraper.dsl.DSL.Extract._
import net.ruippeixotog.scalascraper.dsl.DSL.Parse._
import net.ruippeixotog.scalascraper.model._
import akka.actor._
//import akka.persistence._
//import system.dispatcher
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import org.knowm.xchange.currency._

case class Ping(
    // pingUrl: String, // /backend/notifications/analytics/NXC/poloniex
    coin: String,
    // exchangeUrl: String,
    // http://bittrex.com/Market/Index?MarketName=BTC-AMP
    // https://poloniex.com/#/exchange/btc_HUC
    // https://www.cryptopia.co.nz/Exchange/?market=XSPEC_BTC
    // https://yobit.net/en/trade/XVS/BTC
    exchange: String, valSig: Double, date: Date,
    // max1h: Double, chng1h: Double, max6h: Double,
    // chng6h: Double, max24h: Double, chng24h: Double,
)

// TODO: convert to class to enable logger?
//  extends LazyLogging
object CryptoPingScraper {

  // constants
  val CPingSignup = "https://cryptoping.tech/users/sign_in"
  val CPingList = "https://cryptoping.tech/backend/notifications/current"
  val cpFormat = new java.text.SimpleDateFormat("yyyy-MM-dd HH:mm")
  val utc = TimeZone.getTimeZone("UTC")

  // state
  val browser = JsoupBrowser()
  val pings = collection.mutable.Set.empty[Ping]

  def main(args: Array[String]): Unit = {
    println(s"exchanges: ${Exchanges.exchanges}")
    cpFormat.setTimeZone(utc)
    println(s"exchange coins: ${Exchanges.coins}")
    //    browser.cookieMap ++= List("foo" -> "bar").toMap
    //     ^ TODO: with SNAPSHOT persist cookies and skip login after first time
    val cancelable = Util.system.scheduler.schedule(0 seconds, My.fetchInterval) {
      scrapeCPing()
    }
  }

  def scrapeCPing(): Unit = {
    val doc = getPings(browser)
    println(s"fetched pings")
    val trs: List[Ping] = (doc >> elementList("tr"))
      .drop(1)
      .dropRight(1)
      .take(My.maxPings) // ditch
      .map(parsePing)
      .reverse
    val minAge = System.currentTimeMillis - My.maxAge.toMillis
    val newPings = trs
    .filter((ping: Ping) => {
      // println(s"ping date: ${cpFormat.format(ping.date)}")
      !pings.contains(ping) &&
      ping.date.getTime >= minAge
    })
    pings ++= newPings
    newPings.par.foreach((ping: Ping) => {
      println(s"ping: $ping")
      crypto2xchange(ping.exchange) map Exchanges.exchanges match {
        case None => println(s"skipping exchange ${ping.exchange}")
        case Some(exc) => Exchanges.invest(exc, new Currency(ping.coin), ping.valSig)
      }
    })
  }

  def crypto2xchange(name: String): Option[String] = {
    name match {
      case "Bittrex" => Some("org.knowm.xchange.bittrex.v1.BittrexExchange")
      case "Poloniex" => Some("org.knowm.xchange.poloniex.PoloniexExchange")
      case "Yobit" => Some("org.knowm.xchange.yobit.YoBitExchange")
      // case "Cryptopia" => 
      case s => None
    }
  }

  def getPings(browser: Browser): Document = {
    val maybeListDoc = Util.retryInc {
      browser.get(CPingList)
    }
    maybeListDoc >?> element("table.results") match {
      case Some(tbl) => maybeListDoc
      case None => getPingsLong(browser)
    }
  }

  def getPingsLong(browser: Browser): Document = {
    println("gonna log in first!")
    val creds = My.conf.cryptoping
    val doc = Util.retryInc {
      browser.get(CPingSignup)
    }
    val items: List[Element] = doc >> elementList("#new_user input")
    val form: Map[String, String] = items.map((item: Element) => (
      item >> attr("name"),
      item >?> attr("value") getOrElse ""
    )).toMap ++ List(
      "user[email]" -> creds.user,
      "user[password]" -> creds.pw
    )
    val doc2 = Util.retryInc {
      browser.post(CPingSignup, form)
    }
    val doc3 = Util.retryInc {
      browser.get(CPingList)
    }
    doc3
  }

  def parseCell(el: Element) = {
    val (btcVal, stuff) = (el >> allText).split(" ", 2).toList
      .toHList[String::String::HNil].get.tupled
    (btcVal.toDouble, stuff)
  }
  
  def parseChange(s: String): Double = s.replace("%","").toDouble/100

  def parsePing(el: Element): Ping = {
    val tds = el >> elementList("td")
    val (tdCoin, tdSignal, td1h, td6h, td24h, tdExchange) =
      tds.toHList[Element::Element::Element
        ::Element::Element::Element::HNil].get.tupled
    val (coin, exchange) = Sized(tdCoin, tdExchange)
      .map((el: Element) => el >> text("a")).tupled
    val ((valSig, date), (max1h, chng1h), (max6h, up6h), (max24h, chng24h)) =
      Sized(tdSignal, td1h, td6h, td24h).map(parseCell).tupled
    new Ping(coin, exchange, valSig,
      cpFormat.parse(date),
      // max1h, parseChange(chng1h),
      // max6h, parseChange(up6h),
      // max24h, parseChange(chng24h),
    )
  }

}
