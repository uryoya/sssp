package com.uryoya.sssp

import com.uryoya.sssp.Config.{SsspConfig, ServerConfig}
import pureconfig._

final case class Config (
  sssp: SsspConfig,
  server: ServerConfig,
)

object Config {
  def load: Config = loadConfigOrThrow[Config]

  final case class SsspConfig(
                               timeout: Int, // milli second
                               dsp: List[Dsp],
                             )
  final case class ServerConfig(host: String, port: Int)
  final case class Dsp(adRequest: String, winNotice: String)
}
