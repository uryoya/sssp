package com.uryoya.sssp.entity

import io.circe.{Decoder, Encoder}

final case class AdBidRequest (
  appId: Int,
)

object AdBidRequest {
  implicit val decodeAdBidRequest: Decoder[AdBidRequest] =
    Decoder.forProduct1("app_id")(AdBidRequest.apply)
  implicit val encodeAdBidRequest: Encoder[AdBidRequest] =
    Encoder.forProduct1("app_id")(a => a.appId)
}
