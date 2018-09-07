package com.uryoya.sssp

import java.net.URL

import com.twitter.app.App
import com.twitter.finagle.{Http, Service, http}
import com.twitter.util.{Await, Future}
import com.twitter.conversions.time._
import io.finch._
import io.finch.circe._
import io.circe.{Decoder, Encoder}
import io.circe.generic.auto._
import io.circe.syntax._
import io.circe.parser._

object Server extends App {
  val sspName: String = "SSSP"
  // DSPの情報
  case class Dsp(client: Service[http.Request, http.Response],
                 adRequest: URL,
                 winNotice: URL)

  // HTTP Request / Response
  // SDK-SSP
  case class SSPRequest(appId: Int)
  object SSPRequest {
    implicit val encodeSSPRequest: Encoder[SSPRequest] =
      Encoder.forProduct1("app_id")(a => a.appId)
  }
  case class SSPResponse(url: String)

  // SDK-DSP
  case class DSPRequest(sspName: String,
                        requestTime: java.time.LocalDateTime,
                        requestId: String,
                       appId: Int)
  object DSPRequest {
    val format: java.time.format.DateTimeFormatter =
      java.time.format.DateTimeFormatter.ofPattern("yyyyMMdd-HHMMss.SSSS")
    implicit val encodeDSPRequest: Encoder[DSPRequest] =
      Encoder.forProduct4("ssp_name", "request_time", "request_id", "app_id")(a =>
        (a.sspName, a.requestTime.format(format), a.requestId, a.appId))
  }
  case class DSPResponse(requestId: String, url: String, price: Int)
  object DSPResponse {
    implicit val decodeDSPResponse: Decoder[DSPResponse] =
      Decoder.forProduct3("request_id", "url", "price")(DSPResponse.apply)
  }
  case class WinNotice(requestId: String, price: Int)
  object WinNotice {
    implicit val encodeWinNotice: Encoder[WinNotice] =
      Encoder.forProduct2("request_id", "price")(a => (a.requestId, a.price))
  }
  case class WinResponse(result: String)

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
  def requestToDsp(dsp: Dsp, dSPRequest: DSPRequest): Future[Option[http.Response]] = {
    val request = http.Request(http.Method.Post, dsp.adRequest.getPath)
    request.host = dsp.adRequest.getHost
    request.setContentTypeJson()
    request.setContentString(dSPRequest.asJson.noSpaces)

    dsp
      .client(request)
      .map(Some(_))
      .handle { case _ => None }
  }

  /* WinNoticeを送信する */
  def postWinNotice(dsp: Dsp, winNotice: WinNotice): Future[Option[http.Response]] = {
    val request = http.Request(http.Method.Post, dsp.winNotice.getPath)
    request.host = dsp.winNotice.getHost
    request.setContentTypeJson()
    request.setContentString(winNotice.asJson.noSpaces)

    dsp
      .client(request)
      .map(Some(_))
      .handle { case _ => None }
  }

  // requestIdを作成
  def mkRequestId: String = sspName + "-" + java.util.UUID.randomUUID()

  val dspClients: List[Dsp] = config.sssp.dsp
      .map { dsp =>
        val adRequest = new URL(dsp.adRequest)
        val winNotice = new URL(dsp.winNotice)
        Dsp(mkHttpClient(adRequest), adRequest, winNotice)
      }

  /* 広告配信のエンドポイントを定義 */
  case class Bid(dsp: Dsp, response: DSPResponse) // DSPResponseとDSP情報を紐つけるためのヘルパクラス
  val ad: Endpoint[SSPResponse] =
    post("req" :: jsonBody[SSPRequest]) { req: SSPRequest =>
      // DSPに対してリクエスト
      val requestId = mkRequestId
      Future
        .collect(dspClients.map { dsp =>
          val dspRequest = DSPRequest(sspName, java.time.LocalDateTime.now, requestId, req.appId)
          requestToDsp(dsp, dspRequest).map((dsp, _))
        })
        .map { responses =>
          (for {
            (dsp, maybeResponse) <- responses
            // タイムアウトなどエラーの発生したレスポンスは除外
            response <- maybeResponse
            // HTTP200以外は除外
            if response.status == http.Status.Ok
            // レスポンスボディをJSONデコード
            bid <- decode[DSPResponse](response.getContentString).toOption
            // 有効なレスポンスのみのSeqを生成
          } yield Bid(dsp, bid))
          // DSPからの入札を降順でソート
            .sortBy(_.response.price)(Ordering[Int].reverse).toList match {
            // 一番高い入札を行ったDSPに対して2ndPriceをつけたWinNoticeを送る
            case first :: second :: _ =>
              postWinNotice(first.dsp, WinNotice(requestId, second.response.price))
              Ok(SSPResponse(first.response.url))
            // DSPからのレスポンスが一つしか得られなかった場合、2ndPriceを1円としてWinNoticeを送る
            case one :: Nil =>
              postWinNotice(one.dsp, WinNotice(requestId, 1))
              Ok(SSPResponse(one.response.url))
            // DSPからのレスポンスが1つもなかった場合、自社広告をDSKに対して返す
            case Nil =>
              Ok(SSPResponse("http://mysitead.example.com"))
          }
        }
    }

  def main(): Unit = {
    // サーバーの作成と起動
    val addr = s":${config.server.port}"
    val server = Http.server.serve(addr, ad.toService)

    onExit { Await.result(server.close) }

    Await.ready(server)
  }
}
