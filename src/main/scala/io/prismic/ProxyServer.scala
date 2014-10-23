package io.prismic

import play.api.libs.ws.{WSProxyServer, DefaultWSProxyServer}

/**
 * A proxy.
 */
case class ProxyServer(
  /** The hostname of the proxy server. */
  host: String,

  /** The port of the proxy server. */
  port: Int,

  /** The protocol of the proxy server.  Use "http" or "https".  Defaults to "http" if not specified. */
  protocol: Option[String] = None,

  /** The principal (aka username) of the credentials for the proxy server. */
  principal: Option[String] = None,

  /** The password for the credentials for the proxy server. */
  password: Option[String] = None,

  ntlmDomain: Option[String] = None,

  encoding: Option[String] = None,

  nonProxyHosts: Option[Seq[String]] = None) {
  private[prismic] def asPlayProxyServer: WSProxyServer =
    DefaultWSProxyServer(host, port, protocol, principal, password, ntlmDomain, encoding, nonProxyHosts)
}
