package com.uryoya.sssp

import com.twitter.app.App
import com.twitter.finagle.{Http, Service, http}
import com.twitter.util.{Future, Await}
import com.twitter.conversions.time._
import io.finch._
import io.finch.circe._
//import io.finch.syntax._
import io.circe.{Decoder, Encoder}
import io.circe.generic.auto._
import io.circe.syntax._
import io.circe.parser._

object Server extends App {
  // DSPの情報
  case class Dsp(client: Service[http.Request, http.Response], url: java.net.URL)
  // HTTP Request / Response
  case class AdRequest(appId: Int)
  object AdRequest {
    implicit val decodeAdBidRequest: Decoder[AdRequest] =
      Decoder.forProduct1("app_id")(AdRequest.apply)
    implicit val encodeAdBidRequest: Encoder[AdRequest] =
      Encoder.forProduct1("app_id")(a => a.appId)
  }
  case class AdResponse(url: String)
  case class AdBidRequest(appId: Int)
  object AdBidRequest {
    implicit val decodeAdBidRequest: Decoder[AdBidRequest] =
      Decoder.forProduct1("app_id")(AdBidRequest.apply)
    implicit val encodeAdBidRequest: Encoder[AdBidRequest] =
      Encoder.forProduct1("app_id")(a => a.appId)
  }
  case class AdBidResponse(url: String, price: Int)

  /* FinagleのHTTPクライアントを作成する関数 */
  def mkHttpClient(url: java.net.URL): Service[http.Request, http.Response] = {
    val port = if (url.getPort == -1) url.getDefaultPort else url.getPort
    if (url.getProtocol == "https")
      Http.client
        .withTls(url.getHost)
        .withRequestTimeout(config.sssp.timeout.millisecond)
        .newService(url.getHost + ":" + port)
    else
      Http.client
        .withRequestTimeout(config.sssp.timeout.millisecond)
        .newService(url.getHost + ":" + port)
  }

  /* DSPへのリクエストを作成する関数 */
  def requestToDsp(dsp: Dsp): Future[Option[http.Response]] = {
    val request = http.Request(http.Method.Post, dsp.url.getPath)
    request.host = dsp.url.getHost
    request.setContentTypeJson()
    request.setContentString(AdBidRequest(100).asJson.noSpaces)

    dsp.client(request)
      .map(Some(_))
      .handle { case _ => None }
//      .ensure(dsp.client.close)
  }

  val dspClients = config.sssp.buyers
    .map(urlString => new java.net.URL(urlString))
    .map(url => Dsp(mkHttpClient(url), url))

  /* 広告配信のエンドポイントを定義 */
  val ad: Endpoint[AdResponse] =
    post("ad" :: jsonBody[AdRequest]) { req: AdRequest =>
      Future.collect(dspClients.map(requestToDsp)).map { responses => val bids = for {
          maybeResponse <- responses
          response <- maybeResponse
          if response.status == http.Status.Ok
          bid <- decode[AdBidResponse](response.getContentString).toOption
        } yield bid
        val maybeMaxBidUrl: Option[String] = Option(bids)
          .filter(_.nonEmpty)
          .map(nonEmptyBids => nonEmptyBids.maxBy(_.price).url)
        maybeMaxBidUrl match {
          case Some(maxBidUrl) => Ok(AdResponse(maxBidUrl))
          case None => BadRequest(new Exception) // NoContent
        }
      }
    }

  def main(): Unit = {
    // サーバーの作成と起動
    val addr   = s":${config.server.port}"
    val server = Http.server.serve(addr, ad.toService)

    onExit { Await.result(server.close) }

    Await.ready(server)
  }
}
