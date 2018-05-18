package com.uryoya.sssp.controller

import com.twitter.finagle.{Http, Service}
import com.twitter.finagle.http
import com.twitter.util.Future
import com.twitter.conversions.time._
import com.twitter.finagle.service.TimeoutFilter
import com.twitter.finagle.util.DefaultTimer
import io.circe.syntax._
import io.circe.generic.auto._
import io.circe.parser._
import com.uryoya.sssp.entity.{AdBidRequest, AdBidResponse, AdExhibitRequest, AdExhibitResponse}
import com.uryoya.sssp.config

object AdController {
  val client = mkClient()
  def exhibit(req: AdExhibitRequest): Future[Either[Exception, AdExhibitResponse]] = {
    val requests: Future[Seq[Either[Throwable, http.Response]]] = Future.collect(
      config.sssp.buyers.map(buyer => requestToDsp(new java.net.URI(buyer)))
    )
    requests.map { responses =>
      val bids = for {
        maybeResponse <- responses
        response <- maybeResponse.toOption
        if response.status == http.Status.Ok
        bid <- decode[AdBidResponse](response.getContentString).toOption
      } yield bid
      if (bids.isEmpty)
        Left(new Exception)
      else
        Right(AdExhibitResponse(bids.max.url))
    }
  }

  def example(): Future[AdExhibitResponse] = {
    val req = http.Request(http.Method.Get, "/users/uryoya")
    req.host = "api.github.com"
    client(req).map(r => AdExhibitResponse(r.contentString)).onFailure(println(_))
  }

  private def requestToDsp(url: java.net.URI): Future[Either[Throwable, http.Response]] = {
    // 1. Service[ReqOut, RepIn] のタイムアウトを設定する。
    val timeoutFilter =
      new TimeoutFilter[http.Request, http.Response](config.sssp.timeout.seconds, DefaultTimer.getInstance)
    // 2. クライアント(Service[ReqOut, ReqIn])を作成する。このときTimeoutFilterを加える。
    val client: Service[http.Request, http.Response] = timeoutFilter andThen  Http.client
      // 3. このタイムアウトはHTTPのタイムアウト設定。
      .withRequestTimeout(config.sssp.timeout.millisecond)
      .newService(s"${url.getHost}:${url.getPort}")

    // 4. リクエストボディを構築する。
    val request = http.Request(http.Method.Post, url.getPath)
    request.host = url.getHost
    request.setContentTypeJson
    request.setContentString(AdBidRequest(100).asJson.noSpaces)

    // 5. 例外が送出されるとFuture全体が落ちるので、Eitherで包む。
    client(request)
      .map(Right(_))
      .handle { case t => Left(t) }
      .ensure(client.close())
  }

  private def mkClient(): Service[http.Request, http.Response] = {
    Http.client.withTls("api.github.com").newService("api.github.com:443")
  }
}
