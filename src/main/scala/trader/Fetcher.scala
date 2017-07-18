package trader

import trader._
import trader.Util._
import scala.util._
import scala.collection._
import net.ruippeixotog.scalascraper.browser.JsoupBrowser._
import net.ruippeixotog.scalascraper.browser._
import net.ruippeixotog.scalascraper.dsl._
import net.ruippeixotog.scalascraper.dsl.DSL._
import net.ruippeixotog.scalascraper.dsl.DSL.Extract._
import net.ruippeixotog.scalascraper.dsl.DSL.Parse._
import net.ruippeixotog.scalascraper.model._
import akka.actor._
import akka.http.scaladsl.Http
import akka.http.scaladsl.model._
import akka.http.scaladsl.model.headers._
import akka.http.scaladsl.unmarshalling._
import scala.concurrent._
//import akka.persistence._
//import system.dispatcher
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

object Fetcher {

	// constants
  implicit val system = Util.system
  implicit val materializer = Util.materializer
  val maxAwait = 15 seconds

  // state
  var cookies: /*Seq[HttpHeader]*/ mutable.Map[String, String] = mutable.Map.empty
	// ^ TODO: persist... akka?

  implicit def url2req(url: String) = HttpRequest(uri = url)

  def fetchEntity(req: HttpRequest): Try[HttpEntity] = retryInc {
  	val newReq = cookies.isEmpty match {
  		case true => req
  		case false =>
		  	val header = headers.Cookie(cookies.toSeq: _*)
  			req.withHeaders(header)
  	}
  	// .map(HttpCookiePair.apply _)
    // println(s"newReq ${newReq}")
  	val fut = Http().singleRequest(newReq)
    val resp: HttpResponse = Await.result(fut, maxAwait)
    // println(s"resp ${resp}")
    // resp.header[`Set-Cookie`]
    cookies ++= resp.headers
    .filter(_.isInstanceOf[`Set-Cookie`])
    .map(v => {
      val cookie = v.asInstanceOf[`Set-Cookie`].cookie
      // HttpCookiePair.apply
      (cookie.name, cookie.value)
    })
    // .toSeq
    println(s"${resp.status} ${req.uri.toString}")
    // println(s"resp: $resp")
    // println(s"resp.status.isSuccess: ${resp.status.isSuccess}")
    // println(s"resp.status.isRedirection: ${resp.status.isRedirection}")
    // println(s"resp.status.isFailure: ${resp.status.isFailure}")
    resp.status match {
      case (code: StatusCode) if code.isRedirection => fetchEntity(resp.header[Location].get.uri.toString)
      case (code: StatusCode) if code.isSuccess =>
      	val futEnt = resp.entity.toStrict(maxAwait)
        // println(s"futEnt ${futEnt}")
      	Success(Await.result(futEnt, maxAwait))
      case (code: StatusCode) if code.isFailure => Failure(new Exception(s"http error: $code"))
      case ex => Failure(new Exception(s"error: $ex"))
    }
  }

  def parseString(ent: HttpEntity): String = { 
    Await.result(Unmarshal(ent).to[String], maxAwait)
  }

  def parseJson(ent: HttpEntity): Try[String] = { 
    ent.contentType match {
      case ContentTypes.`application/json` => Success(parseString(ent))
      case _ => Failure(new Exception(s"non-json content type ${ent.contentType}!"))
    }
  }

  def parseHtml(ent: HttpEntity): Try[JsoupDocument] = {
    ent.contentType match {
      case ContentTypes.`text/html(UTF-8)` =>
        val html = parseString(ent)
        Success(JsoupDocument(org.jsoup.Jsoup.parse(html)))
      case _ => Failure(new Exception(s"non-html content type ${ent.contentType}!"))
    }
  }

  def fetchString(req: HttpRequest) = fetchEntity(req)	.map(parseString _)
  def fetchJson(req: HttpRequest) = fetchEntity(req).flatMap(parseJson _)
  def fetchHtml(req: HttpRequest) = fetchEntity(req).flatMap(parseHtml _)

}
