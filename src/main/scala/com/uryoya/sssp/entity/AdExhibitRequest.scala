package com.uryoya.sssp.entity

import io.circe.{Decoder, Encoder}

final case class AdExhibitRequest (
  appId: Int,
)

object AdExhibitRequest {
  implicit val decodeAdBidRequest: Decoder[AdExhibitRequest] =
    Decoder.forProduct1("app_id")(AdExhibitRequest.apply)
  implicit val encodeAdBidRequest: Encoder[AdExhibitRequest] =
    Encoder.forProduct1("app_id")(a => a.appId)
}
