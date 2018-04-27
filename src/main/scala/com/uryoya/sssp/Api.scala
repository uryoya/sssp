package com.uryoya.sssp

import com.twitter.finagle.Service
import com.twitter.finagle.http.{Request, Response}
import com.uryoya.sssp.controller.{AdController, BuyerController}
import com.uryoya.sssp.entity._
import io.circe.generic.auto._
import io.finch.Endpoint
import io.finch._
import io.finch.circe._

class Api {
  val service: Service[Request, Response] = {
    // DSPとして登録する
    val buyerRegistration: Endpoint[BuyerRegistrationResponse] =
      post("buyer" :: "registration" :: jsonBody[BuyerRegistrationRequest]) { req: BuyerRegistrationRequest =>
        BuyerController.registration(req) match {
          case Right(resp) => Ok(resp)
          case Left(err) => BadRequest(err)
        }
      }

    // 広告枠出品
    val adExhibit: Endpoint[AdExhibitResponse] =
      post("ad" :: "exhibit" :: jsonBody[AdExhibitRequest]) { req: AdExhibitRequest =>
        AdController.exhibit(req).map {
          case Right(resp) => Ok(resp)
          case Left(err) => Output.failure(err, com.twitter.finagle.http.Status.MovedPermanently)
        }
      }

    (
      buyerRegistration
        :+: adExhibit
    ).toServiceAs[Application.Json]
  }
}
