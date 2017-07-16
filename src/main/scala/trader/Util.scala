package trader

import scala.util._
import scala.collection.JavaConverters._
// import scala.concurrent.ExecutionContext.Implicits.global
import scala.io._
import cats.syntax.either._
import io.circe._
import io.circe.generic.auto._
import io.circe.generic.semiauto._
import io.circe.generic.decoding._
import io.circe.generic.encoding._
import io.circe.yaml
import io.circe.yaml.parser
import akka.actor._
import akka.stream._

object Util {

  implicit val system = ActorSystem()
  implicit val materializer = ActorMaterializer()
  // implicit val executionContext = system.dispatchers.lookup("my-dispatcher")

  // def jsonAs[T](json: Json): T = {
  //   val // derived: Lazy[DerivedDecoder[T]] = DerivedDecoder.deriveDecoder[T, R]
  //   val decoder: Decoder[T] = deriveDecoder[T]
  //   json
  //   .flatMap(_.as[T](decoder))
  //   .valueOr(throw _)
  // }

  def writeFile(file: String, str: String) = {
    val fPath = getClass.getClassLoader.getResource(file).getPath
    val writer = new java.io.PrintWriter(fPath)
    try {
      writer.write(str)
    } finally {
      writer.close
    }
  }

  def yamlFile(file: String): Either[ParsingFailure, Json] = {
    val fPath = getClass.getClassLoader.getResource(file).getPath
    val yaml = Source.fromFile(fPath).mkString
    parser.parse(yaml)
  }

  def kestrel[A](x: A)(f: A => Unit): A = { f(x); x } // tap
  def log[A](x: A) = kestrel(x)(println)
  def log[A](s: String, x: A) = kestrel(x){ y => println(s"$s: $y") }

  // Returning T, throwing the exception on failure
  @annotation.tailrec
  def retryFail[T](n: Int)(fn: => T): T = {
    Try { fn } match {
      case Success(x) => x
      case _ if n > 1 => retryFail(n - 1)(fn)
      case Failure(e) => throw e
    }
  }

  // Returning a Try[T] wrapper
  @annotation.tailrec
  def retry[T](n: Int)(fn: => T): Try[T] = {
    Try { fn } match {
      case (x: Success[T]) => x
      case Failure(ex) if n > 1  =>
        println(ex)
        println(s"failed, attempts left: ${n-1}")
        retry(n - 1)(fn)
      case f => f
    }
  }

  @annotation.tailrec
  def retryInc[T](fn: => T, n: Int = 1): T = {
    Try { fn } match {
      case Success(x) => x
      case Failure(ex) =>
        println(ex)
        // println(s"msg: ${ex.getMessage}")
        val interval = math.pow(2, n).asInstanceOf[Number].longValue
        println(s"retry in ${interval}s")
        Thread.sleep(interval * 1000)
        retryInc(fn, n+1)
    }
  }

}
