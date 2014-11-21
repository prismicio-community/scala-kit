package io.prismic

import java.net.URI
import java.util.concurrent.Executors

import io.netty.bootstrap.Bootstrap
import io.netty.channel.nio.NioEventLoopGroup
import io.netty.channel.socket.SocketChannel
import io.netty.channel.socket.nio.NioSocketChannel
import io.netty.channel.{ChannelPipeline, ChannelHandlerContext, ChannelInitializer, SimpleChannelInboundHandler}
import io.netty.handler.codec.http._
import io.netty.handler.ssl.SslContext
import io.netty.handler.ssl.util.InsecureTrustManagerFactory
import io.netty.util.CharsetUtil
import play.api.libs.json.{JsValue, Json}

import scala.collection.JavaConversions._
import scala.concurrent.{Future, Promise}
import scala.util.control.Exception._


case class ClientResponse(
  status: HttpResponseStatus,
  body: String,
  headers: HttpHeaders
) {
  def json: JsValue = Json.parse(body)
  def header(key: String): Seq[String] = headers.getAll(key)
}

object HttpClient {

  private val ThreadCount = getIntProperty("PRISMIC_MAX_CONNECTIONS").getOrElse(20)
  private[prismic] val UserAgent = s"Prismic-${Info.name}/${Info.version} Scala/${Info.scalaVersion} JVM/${System.getProperty("java.version")}"
  private[prismic] val AcceptJson = Seq("Accept" -> "application/json")
  private[prismic] val MaxAge = """max-age\s*=\s*(\d+)""".r

  val group = new NioEventLoopGroup(ThreadCount)

  def getJson(url: String,
              headers: Map[String, Any] = Map.empty,
              proxy: Option[ProxyServer] = None): Future[ClientResponse] =
    get(url, headers ++ Map("Accept" -> "application/json"), proxy)

  def get(url: String,
          headers: Map[String, Any] = Map.empty,
          proxy: Option[ProxyServer] = None): Future[ClientResponse] = {

    val result = Promise[ClientResponse]()

    var body = ""
    var status: HttpResponseStatus = null
    var responseHeaders: HttpHeaders = null

    val uri = new URI(url)
    val scheme = Option(uri.getScheme).getOrElse("http")
    val host = Option(uri.getHost).getOrElse("127.0.0.1")
    val port = (uri.getPort, scheme.toLowerCase) match {
      case (-1, "http") => 80
      case (-1, "https") => 443
      case (-1, _) => sys.error("Only HTTP(S) is supported")
      case (p, _) => p
    }
    val fullPath = uri.getRawPath + Option(uri.getRawQuery).map("?" + _).getOrElse("")

    val b = new Bootstrap()
    b.group(group)
      .channel(classOf[NioSocketChannel])
      .handler(new ChannelInitializer[SocketChannel] {
      override def initChannel(ch: SocketChannel) = {
        val p: ChannelPipeline = ch.pipeline()

        if (scheme == "https") {
          val sslCtx = SslContext.newClientContext(SslContext.defaultClientProvider())
          p.addLast("ssl", sslCtx.newHandler(ch.alloc(), host, port))
        }

        p.addLast(new HttpClientCodec())

        p.addLast(new HttpContentDecompressor())

        // Uncomment the following line if you don't want to handle HttpContents.
        p.addLast(new HttpObjectAggregator(1048576))

        p.addLast(new SimpleChannelInboundHandler[HttpObject] {
          override def channelRead0(ctx: ChannelHandlerContext, msg: HttpObject) {
            msg match {
              case response: FullHttpResponse =>
                result.success(ClientResponse(
                  response.getStatus,
                  response.content().toString(CharsetUtil.UTF_8),
                  response.headers())
                )
                ctx.close()
              case response: HttpResponse =>
                status = response.getStatus
                responseHeaders = response.headers()
              case content: LastHttpContent =>
                body += content.content().toString(CharsetUtil.UTF_8)
                ctx.close()
                result.success(ClientResponse(status, body, responseHeaders))
              case content: HttpContent =>
                body += content.content().toString(CharsetUtil.UTF_8)
              case _ => ()
            }
          }

          override def exceptionCaught(ctx: ChannelHandlerContext, cause: Throwable) {
            cause.printStackTrace()
            ctx.close()
            result.failure(cause)
          }
        })
      }
    })

    // Make the connection attempt.
    val ch = b.connect(host, port).sync().channel()

    // Prepare the HTTP request.
    val request = new DefaultFullHttpRequest(HttpVersion.HTTP_1_1, HttpMethod.GET, fullPath)
    (headers ++ Map(
      HttpHeaders.Names.USER_AGENT -> UserAgent,
      HttpHeaders.Names.HOST -> host,
      HttpHeaders.Names.CONNECTION -> HttpHeaders.Values.CLOSE,
      HttpHeaders.Names.ACCEPT_ENCODING -> HttpHeaders.Values.GZIP
    )).map { case (key, value) =>
      request.headers().set(key, value)
    }

    // Send the HTTP request.
    ch.writeAndFlush(request)

    // Wait for the server to close the connection.
    ch.closeFuture().sync()

    result.future
  }

  private def getIntProperty(key: String): Option[Int] = Option(System.getProperty(key)).flatMap { strValue =>
    catching(classOf[NumberFormatException]) opt strValue.toInt
  }

}