package com.uryoya.sssp.controller

import com.twitter.finagle.{Http, Service}
import com.twitter.finagle.http
import com.twitter.util.{Await, Future, Duration}
import com.twitter.conversions.time._
import io.circe.Encoder
import io.circe.syntax._
import io.circe.generic.auto._
import io.circe.parser._
import com.uryoya.sssp.entity.{AdBidRequest, AdBidResponse, AdExhibitRequest, AdExhibitResponse}
import com.uryoya.sssp.config

object AdController {
  def exhibit(req: AdExhibitRequest): Future[Either[Exception, AdExhibitResponse]] = {
    val requests: List[Future[http.Response]] = config.sssp.buyers.map { buyer =>
      val url = new java.net.URI(buyer)
      val client: Service[http.Request, http.Response] = Http.client
        .withRequestTimeout(config.sssp.timeout.millisecond)
        .newService(s"${url.getHost}:${url.getPort}")
      val request = http.Request(http.Method.Post, url.getPath)
      request.host = url.getHost
      request.setContentTypeJson
      request.setContentString(AdBidRequest(100).asJson.noSpaces)
      client(request)
    }
    Await.ready(Future.collect(requests), config.sssp.timeout.milliseconds).map { responses =>
      val bids = for {
        response <- responses
        if response.status == http.Status.Ok
        bid <- decode[AdBidResponse](response.getContentString).toOption
      } yield bid
      if (bids.length == 0)
        Left(new Exception)
      else
        Right(AdExhibitResponse(bids.max.url))
    }
  }
}
