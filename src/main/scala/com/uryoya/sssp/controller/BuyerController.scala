package com.uryoya.sssp.controller

import com.uryoya.sssp.entity.{BuyerRegistrationRequest, BuyerRegistrationResponse}

object BuyerController {
  def registration(req: BuyerRegistrationRequest): Either[Exception, BuyerRegistrationResponse] =
    Right(BuyerRegistrationResponse(buyerId = 1))
}
