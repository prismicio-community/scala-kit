package io.prismic

/**
 * A proxy.
 */
case class ProxyServer(
  /** The hostname of the proxy server. */
  host: String,

  /** The port of the proxy server. */
  port: Int,

  @deprecated("Ignored", "1.3.4")
  protocol: Option[String] = None,

  /** The principal (aka username) of the credentials for the proxy server. */
  principal: Option[String] = None,

  /** The password for the credentials for the proxy server. */
  password: Option[String] = None,

  @deprecated("Ignored", "1.3.4")
  ntlmDomain: Option[String] = None,

  @deprecated("Ignored", "1.3.4")
  encoding: Option[String] = None,

  @deprecated("Ignored", "1.3.4")
  nonProxyHosts: Option[Seq[String]] = None) {

}
