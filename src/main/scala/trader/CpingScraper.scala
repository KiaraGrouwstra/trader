package trader

import trader._
import trader.Util._
import scala.util._
// import java.util._
import java.util.{Date,Calendar,TimeZone}
//import monocle._
//import monocle.macros._
// import monocle.macros.syntax.lens._
import io.circe._, io.circe.generic.auto._, io.circe.parser._, io.circe.syntax._
import io.circe.generic.semiauto._
import io.circe.generic.JsonCodec
// import io.circe.generic.extras.defaults._
import shapeless._
import shapeless.syntax.std.tuple._
import shapeless.syntax.std.traversable._
import shapeless.syntax.std.maps._
// import net.ruippeixotog.scalascraper.browser.JsoupBrowser._
// import net.ruippeixotog.scalascraper.browser._
import net.ruippeixotog.scalascraper.dsl._
import net.ruippeixotog.scalascraper.dsl.DSL._
import net.ruippeixotog.scalascraper.dsl.DSL.Extract._
import net.ruippeixotog.scalascraper.dsl.DSL.Parse._
import net.ruippeixotog.scalascraper.model._
import akka.actor._
// import akka.http.scaladsl.Http
// import akka.http.scaladsl.model._
// import akka.http.scaladsl.model.headers.Location
// import scala.concurrent.Future
//import akka.persistence._
//import system.dispatcher
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import org.knowm.xchange.currency._
// import akka.http.scaladsl.Http
import akka.http.scaladsl.model._
import akka.http.scaladsl.model.HttpMethods._
import akka.http.scaladsl.model.HttpProtocols._
// import akka.http.scaladsl.model.headers._

object CpingScraper {

  // types
  type CandleTuple = 
  List[Double]
  // Either[
  //   (Long, Double, Double, Double, Double),
  //   (Long, Double, Double, Double, Double, Double, Double, Double),
  // ]
  // (Long, Double, Double, Double, Double, Double, Double, Double)
  case class PingStat(max: Double, change: Double) //, period: Long
  // @JsonCodec 
  case class Ping(
      // pingUrl: String, // /backend/notifications/analytics/NXC/poloniex
      // https://cryptoping.tech/backend/chart_data/NXC/poloniex
      coin: String,
      // exchangeUrl: String,
      // http://bittrex.com/Market/Index?MarketName=BTC-AMP
      // https://poloniex.com/#/exchange/btc_HUC
      // https://www.cryptopia.co.nz/Exchange/?market=XSPEC_BTC
      // https://yobit.net/en/trade/XVS/BTC
      exchange: String, valSig: Double, date: Long,
      // max1h: Double, chng1h: Double, max6h: Double,
      // chng6h: Double, max24h: Double, chng24h: Double,
      stats: List[PingStat]
  )
  case class CPingEnvelope[T](error: Boolean, result: T)
  case class PingChart(ticker: String, exchange: String, candles: List[CandleTuple])
  case class Candle(timestamp: Long, open: Double, high: Double, low: Double,
    close: Double, volume: Double = 0.0, buy: Double = 0.0, sell: Double = 0.0)
  type CandleMap = List[(Ping, List[Candle])]

  implicit val candleTupleEncoder:  Encoder[CandleTuple] =  deriveEncoder
  implicit val pingEncoder:         Encoder[Ping] =         deriveEncoder
  implicit val pingChartEncoder:    Encoder[PingChart] =    deriveEncoder
  implicit val candleEncoder:       Encoder[Candle] =       deriveEncoder
  implicit val candleMapEncoder:    Encoder[CandleMap] =    deriveEncoder

  implicit def url2req(url: String) = HttpRequest(uri = url)

  // constants
  val cPingSignup = "https://cryptoping.tech/users/sign_in"
  val cPingList = "https://cryptoping.tech/backend/notifications/current"
  def cPingHistory(i: Int) = s"https://cryptoping.tech/backend/notifications?page=${i}"
  def cPingStats(ping: Ping) = s"https://cryptoping.tech/backend/chart_data/${ping.coin}/${ping.exchange.toLowerCase}"
  val utc = TimeZone.getTimeZone("UTC")
  val cpFormat = {
    val f = new java.text.SimpleDateFormat("yyyy-MM-dd HH:mm")
    f.setTimeZone(utc)
    f
  }

  // state
  val pingSet = collection.mutable.Set.empty[Ping]

  def main(args: Array[String]): Unit = {
    // checkHistory
    val exchanges = new Exchanges(List(
      "org.knowm.xchange.bittrex.v1.BittrexExchange",
      // "org.knowm.xchange.yobit.YoBitExchange",
      // "org.knowm.xchange.poloniex.PoloniexExchange",
    ))
    // println(s"exchanges: ${exchanges.exchanges}")
    // println(s"exchange coins: ${exchanges.coins}")
    val cancelable = system.scheduler.schedule(0 seconds, My.conf.pingInterval seconds) {
      scrapeCPing(exchanges)
    }
  }

  def scrapeCPing(exchanges: Exchanges): Unit = {
    val doc = fetchCPingHtml(cPingList)
    val pings: List[Ping] = (doc >> elementList("tr"))
      .drop(1)
      .dropRight(1)
      .map(parsePing)
      .reverse
    val minAge = System.currentTimeMillis - (My.conf.maxAge minutes).toMillis
    val newPings = pings
    .filter((ping: Ping) => {
      // println(s"ping date: ${cpFormat.format(ping.date)}")
      !pingSet.contains(ping) &&
      ping.date >= minAge
    })
    pingSet ++= newPings
    newPings.par.foreach((ping: Ping) => {
      println(s"ping: $ping")
      crypto2xchange(ping.exchange).flatMap((s: String) => Option(exchanges.exchanges(s))) match {
        case None => println(s"skipping exchange ${ping.exchange}")
        case Some(exc) => exchanges.invest(exc, new Currency(ping.coin), ping.valSig)
      }
    })
  }

  def checkHistory(): Unit = {
    val pings = fetchHistory()
    println(s"fetched history")
    val map = pings.map((ping: Ping) => {
      println(s"fetching candles for ping ${ping}")
      // val candles = Fetcher.parseJson(fetchCPing(cPingStats(ping)))
      val parsed = Fetcher.parseJson(fetchCPing(cPingStats(ping)))
      // println(s"parsed: $parsed")
      val decoded = parsed
        // .map(s => log("fetched and parsed", s))
        .toEither
        .flatMap(decode[CPingEnvelope[PingChart]] _)
      // println(s"decoded: $decoded")
      val candles = decoded
        // .map(s => log("decoded", s))
        .toOption.get
        .result.candles.map((numbers: CandleTuple) => {
          // either.fold(tpl5 => tpl5 ::: (0.0,0.0,0.0), identity)
          val timestamp = numbers(0).toLong
          val doubles = numbers.drop(1)
          val tpl =
            if (doubles.size == 4)
              (timestamp :: doubles.toHList[Double::Double::Double
                  ::Double::HNil].get ::: (0.0 :: 0.0 :: 0.0 :: HNil)).tupled
            else if (doubles.size == 5)
              (timestamp :: doubles.toHList[Double::Double::Double::Double
                  ::Double::HNil].get ::: (0.0 :: 0.0 :: HNil)).tupled
            else // if (doubles.size == 7)
              (timestamp :: doubles.toHList[Double::Double::Double
                  ::Double::Double::Double::Double::HNil].get).tupled
          Candle.tupled(tpl)
        })
        // .map(s => log("candled", s))
      // println(s"candles: $candles")
      println(s"candles: ${candles.size}")
      (ping, candles)
    }) //.toMap
    // implicit val encoder: Encoder[CandleMap] = deriveEncoder[CandleMap]
    val json = map.asJson.noSpaces
    // println(s"json: ${json}")
    writeFile("history.json", json)
    println(s"wrote history")
  }

  def fetchHistory(page: Int = 1, acc: List[Ping] = Nil): List[Ping] = {
    println(s"fetching history page $page...")
    val url = cPingHistory(page)
    val doc = fetchCPingHtml(url)
    val pings: List[Ping] = (doc >> elementList("tr"))
      .drop(1)
      .map(parsePing)
    if (pings.isEmpty)
      acc
    else
      fetchHistory(page+1, acc ++ pings)
  }

  def crypto2xchange(name: String): Option[String] = {
    name match {
      case "bittrex" => Some("org.knowm.xchange.bittrex.v1.BittrexExchange")
      case "poloniex" => Some("org.knowm.xchange.poloniex.PoloniexExchange")
      case "yobit" => Some("org.knowm.xchange.yobit.YoBitExchange")
      // case "cryptopia" => 
      case s => None
    }
  }

  def fetchCPingHtml(url: String): Document =
    Fetcher.parseHtml(fetchCPing(url)).get

  def fetchCPing(url: String): HttpEntity = {
    val ent: Try[HttpEntity] = Fetcher.fetchEntity(url)
    // .merge
    println(s"fetched pings")
    // println(s"ent: ${ent}")
    ent.flatMap(Fetcher.parseHtml _) match {
      case Success(doc) =>
        // println(s"doc: ${doc}")
        val el = doc >?> element("#new_user")
        // println(s"el: ${el}")
        el match {
          case Some(form) => authThenFetch(url, doc)
          case None => ent.get
        }
      case _ => ent.get // json i/o login page, yay
    }
  }

  def authThenFetch(url: String, doc: Document): HttpEntity = {
    println("gonna log in first!")
    val creds = My.conf.cryptoping
    val items: List[Element] = doc >> elementList("#new_user input")
    val form: Map[String, String] = items.map((item: Element) => (
      item >> attr("name"),
      item >?> attr("value") getOrElse ""
    )).toMap ++ List(
      "user[email]" -> creds.user,
      "user[password]" -> creds.pw
    )
    val formData = FormData(form).toEntity
    Fetcher.fetchHtml(new HttpRequest(POST, cPingSignup, Nil, formData, `HTTP/1.0`))
    // ^ get url from form action?
    fetchCPing(url)
  }

  def parseCell(el: Element) = {
    val (btcVal, stuff) = (el >> allText).split(" ", 2).toList
      .toHList[String::String::HNil].get.tupled
    (btcVal.toDouble, stuff)
  }
  
  def parseChange(s: String): Double = s.replace("%","").toDouble/100

  def parsePing(el: Element): Ping = {
    // println(s"parsing tr: ${el}")
    val tds = el >> elementList("td")
    // println(s"tds: ${tds}")
    val tdCoin = tds(0)
    val tdSignal = tds(1)
    val tdExchange = tds.last
    val statTds = tds.drop(2).dropRight(1)
    // ^ current: 1h, 6h, 24h; history: also 48h, 7d
    val coin = tdCoin >> text("td")
    val exchange = tdExchange >> text("td")
    val (valSig, date) = parseCell(tdSignal)
    val time = cpFormat.parse(date).getTime
    val stats = statTds
      .map(parseCell)
      .map(tpl => (tpl._1, parseChange(tpl._2)))
      .map(PingStat.tupled)
    new Ping(coin, exchange.toLowerCase, valSig, time, stats)
  }

}
