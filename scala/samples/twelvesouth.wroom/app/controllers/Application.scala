package controllers

import scala.concurrent._

import play.api._
import play.api.mvc._
import play.api.cache._
import play.api.libs.ws._

import play.api.libs.json._
import play.api.libs.functional.syntax._

import play.api.Play.current
import scala.concurrent.ExecutionContext.Implicits.global

import com.zenexity.wroom.client._

object Application extends Controller {

  def index = PlayWroom.withApi(Scope.Master){ api =>
    Action {
      Async {
        /*val form = api.master.form("document")
           .withFields(
             "id" -> "AAAAAAdbzTgAAAAA"/*,
             "ref" -> "AAAAAAdbzT8AAAAA"*/
           )
           .withWidgets("article.body" -> Render.HTML)

         println("form.action:"+form.form.action+ "qs:"+form.queryString)

        form.submit
           .map{ docs =>
             val doc = docs.head
             Ok(views.html.index(doc.id, doc("article.body").asHTML.content))
           }*/
/*{
  "product.name": {
    type: "StructureText",
    data: "<h1>Tapette à mouche </h1>"
  },

  "product.image": {
    type: "Image",
    "data" : {
      "main" : {
        "dimensions" : ,
        "image"
      }
    }
  }
}*/
        api.form("documents").withRef(api.master)
           .withFields("types" -> Seq("product"))
           .withChunks("_" -> Render.HTML)
           .submit
           .map{ products =>
              Ok(views.html.index("Twelvesouth Products", products))
           }

      }
    }
  }

}



sealed trait Scope

object Scope {
  case object Master extends Scope {
    override def toString = "master"
  }
  case object All extends Scope {
    override def toString = "master+releases"
  }
}


object PlayWroom extends Controller {
  import com.zenexity.wroom.client.{Wroom, Api}

  object Conf {
    val SERVER_URL = current.configuration.getString("wroom.server_url").get
    val CLIENT_ID = current.configuration.getString("wroom.client_id").get
    val CLIENT_SECRET = current.configuration.getString("wroom.client_secret").get
  }

  def withApi(scope: Scope)(f: Api => Action[AnyContent]): Action[AnyContent] = withApi(play.api.mvc.BodyParsers.parse.anyContent)(scope)(f)

  def withApi[A](p: BodyParser[A])(scope: Scope)(f: Api => Action[A]): Action[A] = {
    withOAuth(p)(scope) { access_token =>
      Action(p){ request:Request[A] =>
        Async {
          for {
            api <- Wroom.url(s"http://${Conf.SERVER_URL}")
                        .withQueryString("access_token" -> access_token)
                        .api
          } yield { f(api)(request) }
        }
      }
    }
  }

  val CACHE_TOKEN_KEY = "tok"

  def withOAuth[A](p: BodyParser[A])(scope: Scope)(f: String => Action[A]): Action[A] = {
    Cache.getAs[String](CACHE_TOKEN_KEY) match {
      case None =>
        Action(p) { request:Request[A] =>
          val queryString = Map(
            "client_id"    -> Seq(Conf.CLIENT_ID),
            "redirect_uri" -> Seq(s"http://${request.host}/oauth/return?orig=${request.uri}"),
            "scope"        -> Seq(scope.toString),
            "response_type" -> Seq("code")
          )

          Redirect(s"http://${Conf.SERVER_URL}/auth", queryString)
        }

      case Some(access_token) =>
        f(access_token)
    }
  }

  val returnReads = (
    (__ \ "access_token").read[String] and
    (__ \ "expires_in").read[Int]
  ).tupled

  def oauthReturnUrl(code: Option[String], orig: String, state: Option[String], error: Option[String], error_description: Option[String]) = Action{
    error match {
      case None =>
        Async {
          WS.url(s"http://${Conf.SERVER_URL}/auth/token").post(Map(
            "client_id"     -> Seq(Conf.CLIENT_ID),
            "client_secret" -> Seq(Conf.CLIENT_SECRET),
            "code"          -> Seq(code.get)
          )).map{ resp =>
            returnReads.reads(resp.json).map{
              case (access_token, expires_in) =>
                Cache.set(CACHE_TOKEN_KEY, access_token, expires_in)
                Redirect(orig)
            }.recoverTotal{ e => BadRequest("OAuth2 flow couldn't be ended correctly") }
          }
        }
      case Some(error) => Unauthorized(error)
    }
  }

}

