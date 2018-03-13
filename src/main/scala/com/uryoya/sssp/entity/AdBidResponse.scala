package com.uryoya.sssp.entity

final case class AdBidResponse (url: String, price: Int)

object AdBidResponse {
  import scala.math._
  implicit val ordering: Ordering[AdBidResponse] =
    Ordering.by(bid => bid.price)
  implicit def orderingToOrdered(bid: AdBidResponse) =
    Ordered.orderingToOrdered(bid)(ordering)
}
