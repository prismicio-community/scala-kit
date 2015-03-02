package io.prismic

import org.joda.time._

import io.prismic.Fragment._
import spray.json._

import scala.util.Try


object PrismicJsonProtocol extends DefaultJsonProtocol with NullOptions {

  import PrismicJson._

  implicit object WeblinkReader extends RootJsonReader[WebLink] {
    override def read(json: JsValue): WebLink = json.asJsObject.getFields("url") match {
      case Seq(JsString(url)) => WebLink(url)
      case _ => throw new DeserializationException("Expected url field")
    }
  }

  implicit object MediaLinkReader extends RootJsonReader[MediaLink] {
    override def read(json: JsValue): MediaLink = (json \ "file").asJsObject.getFields("url", "kind", "size", "name") match {
      case Seq(JsString(url), JsString(kind), JsString(size), JsString(name)) => MediaLink(url, kind, size.toLong, name)
      case _ => throw new DeserializationException("Missing field")
    }
  }

  implicit object DocumentLinkReader extends RootJsonReader[DocumentLink] {
    override def read(json: JsValue): DocumentLink = {
      val uid = (json \ "document" \ "uid").toOpt[String]
      val tags = (json \ "document" \ "tags").toOpt[Seq[String]].getOrElse(Nil)
      val isBroken = (json \ "isBroken").toOpt[Boolean].getOrElse(false)
      val maybeDoc: Either[Throwable, DocumentLink] = for {
        id <- (json \ "document" \ "id").validate[String].right
        slug <- (json \ "document" \ "slug").validate[String].right
        typ <- (json \ "document" \ "type").validate[String].right
        data = DocumentReader.parseFragments((json \ "document" \ "data" \ typ).asJsObject, typ)
      } yield {
        DocumentLink(id, uid, typ, tags, slug, data, isBroken)
      }
      maybeDoc match {
        case Left(t) => throw t
        case Right(docLink) => docLink
      }

    }

  }

  implicit object DateReader extends RootJsonReader[Date] {
    override def read(json: JsValue): Date =
      Try(Date(LocalDate.parse(json.asInstanceOf[String], format.DateTimeFormat.forPattern("yyyy-MM-dd")))).getOrElse {
        throw new DeserializationException("date parsing error")
    }
  }

  implicit object TextReader extends RootJsonReader[Text] {
    override def read(json: JsValue): Text = Text(json.asInstanceOf[String])
  }

  implicit object TimestampReader extends RootJsonReader[Timestamp] {
    override def read(json: JsValue): Timestamp = {
      val d = json.convertTo[String]
        val isoFormat = org.joda.time.format.DateTimeFormat.forPattern("yyyy-MM-dd'T'HH:mm:ssZ")
        Try(Timestamp(DateTime.parse(d, isoFormat).withZone(DateTimeZone.UTC)))
          .getOrElse(throw new DeserializationException(s"Invalid timestamp value $d"))
    }
  }

  implicit object NumberReader extends RootJsonReader[Number] {
    override def read(json: JsValue): Number = Number(json.asInstanceOf[Double])
  }

  implicit object ColorReader extends RootJsonReader[Color] {
    override def read(json: JsValue): Color = json match {
      case JsString(hex) =>
        if (Color.isValidColorValue(hex)) {
          Color(hex)
        } else {
          throw new DeserializationException(s"Invalid color value $hex")
        }
      case _ => throw new DeserializationException(s"Expected String")
    }
  }

  implicit object EmbedReader extends RootJsonReader[Embed] {
    override def read(json: JsValue): Embed = json.asJsObject.getFields("oembed") match {
      case Seq(oembed) => oembed.asJsObject.getFields("type", "provider_name", "embed_url", "width", "height", "html") match {
        case (JsString(typ), provider:JsValue, JsString(url), width:JsValue, height:JsValue, html) =>
          Embed(typ, provider.convertTo[Option[String]], url, width.convertTo[Option[Int]], height.convertTo[Option[Int]], html, oembed)
      }
    }
  }

  implicit object GeoPointReader extends RootJsonReader[GeoPoint] {
    override def read(json: JsValue): GeoPoint = json.asJsObject.getFields("latitude", "longitude") match {
      case Seq(JsNumber(latitude), JsNumber(longitude)) => GeoPoint(latitude.toDouble, longitude.toDouble)
      case _ => throw new DeserializationException("Expected longitude and latitude as numbers")
    }
  }

  def headingReader(level: Int) = new RootJsonReader[StructuredText.Block.Heading] {
    override def read(json: JsValue) = StructuredText.Block.Heading(
      (json \ "text").convertTo[String],
      (json \ "spans").toOpt[Seq[StructuredText.Span]].getOrElse(Nil),
      level,
      (json \ "label").toOpt[String],
      (json \ "direction").toOpt[String]
    )
  }

  def listItemReader(ordered: Boolean) = new RootJsonReader[StructuredText.Block.ListItem] {
    override def read(json: JsValue) = StructuredText.Block.ListItem(
      (json \ "text").convertTo[String],
      (json \ "spans").toOpt[Seq[StructuredText.Span]].getOrElse(Nil),
      ordered,
      (json \ "label").toOpt[String],
      (json \ "direction").toOpt[String]
    )
  }

  implicit object EmbedReader extends RootJsonReader[StructuredText.Block.Embed] {
    override def read(json: JsValue) = StructuredText.Block.Embed(
      json.convertTo[Embed],
      (json \ "label").toOpt[String],
      (json \ "direction").toOpt[String]
    )
  }

  implicit object BlockReader extends RootJsonReader[StructuredText.Block] {
    override def read(json: JsValue): StructuredText.Block =
      json.asJsObject.getFields("type", "value") match {
        case Seq(JsString("heading1"), value) => value.convertTo[StructuredText.Block.Heading](headingReader(1))
        case Seq(JsString("heading2"), value) => value.convertTo[StructuredText.Block.Heading](headingReader(2))
        case Seq(JsString("heading3"), value) => value.convertTo[StructuredText.Block.Heading](headingReader(3))
        case Seq(JsString("heading4"), value) => value.convertTo[StructuredText.Block.Heading](headingReader(4))
        case Seq(JsString("paragraph"), value) => value.convertTo[StructuredText.Block.Paragraph]
        case Seq(JsString("preformatted"), value) => value.convertTo[StructuredText.Block.Preformatted]
        case Seq(JsString("list-item"), value) => value.convertTo[StructuredText.Block.ListItem](listItemReader(ordered = false))
        case Seq(JsString("o-list-item"), value) => value.convertTo[StructuredText.Block.ListItem](listItemReader(ordered = true))
        case Seq(JsString("image"), value) => value.convertTo[StructuredText.Block.Image]
        case Seq(JsString("embed"), value) => value.convertTo[StructuredText.Block.Embed]
        case t => throw new DeserializationException(s"Unsupported block type $t")
      }
  }

  implicit object StructuredTextReader extends RootJsonReader[StructuredText] {
    override def read(json: JsValue): StructuredText = json match {
      case JsArray(elements) => StructuredText(elements.map(_.convertTo[StructuredText.Block]))
      case _ => throw new DeserializationException("Expected JsArray")
    }
  }

  implicit object DocumentReader extends RootJsonReader[Document] {
    override def read(jsValue: JsValue): Document = {
      val json = jsValue.asJsObject
      json.getFields("id", "href", "type", "data") match {
        case Seq(JsString(id), JsString(href), JsString(typ), data: JsObject) =>
          val uid: Option[String] = (json \ "uid").toOpt[String]
          val tags: Seq[String] = (json \ "tags").toOpt[Seq[String]].getOrElse(Nil)
          val slugs: Seq[String] = (json \ "tags").toOpt[Seq[String]].getOrElse(Nil).map(decode)
        case _ => throw new DeserializationException("Expected id, href, type and data")
      }
    }

    def parseFragments(json: JsObject, typ: String): Map[String, Fragment] = {
      val fields: Iterable[(String, Fragment)] = json.fields.map {
        case (key, json: JsObject) => parse(json).toList.map(fragment => (s"$typ.$key", fragment))
        case (key, jsons: JsArray) => jsons.elements.zipWithIndex.collect {
          case (json: JsObject, i) => parse(json).toList.map(fragment => (s"$typ.$key[$i]", fragment))
          case _ => Nil
        }.flatten
        case _ => Nil
      }.flatten.toSeq
      collection.immutable.ListMap(fields:_*)
    }

    private def parse(jsvalue: JsObject): Option[Fragment] = {
      jsvalue.getFields("type", "value") match {
        case Seq(JsString("Image"), value) => Some(value.convertTo[Fragment.Color])
        case Seq(JsString("Number"), value) => Some(value.convertTo[Fragment.Number])
        case Seq(JsString("Date"), value) => Some(value.convertTo[Fragment.Date])
        case Seq(JsString("Timestamp"), value) => Some(value.convertTo[Fragment.Timestamp])
        case Seq(JsString("GeoPoint"), value) => Some(value.convertTo[Fragment.GeoPoint])
        case Seq(JsString("Text"), value) => Some(value.convertTo[Fragment.Text])
        case Seq(JsString("Select"), value) => Some(value.convertTo[Fragment.Text])
        case Seq(JsString("Embed"), value) => Some(value.convertTo[Fragment.Embed])
        case Seq(JsString("Link.web"), value) => Some(value.convertTo[Fragment.WebLink])
        case Seq(JsString("Link.document"), value) => Some(value.convertTo[Fragment.DocumentLink])
        case Seq(JsString("Link.file"), value) => Some(value.convertTo[Fragment.MediaLink])
        case Seq(JsString("StructuredText"), value) => Some(value.convertTo[Fragment.StructuredText])
        case Seq(JsString("Group"), value) => Some(value.convertTo[Fragment.Group])
        case _                => None
      }
    }

    private def decode(slugs: Seq[String]) = slugs.map(java.net.URLDecoder.decode(_, "UTF-8"))

  }

  implicit val responseFormat = jsonFormat8(Response)

  // API Related classes

  implicit object ApiDataReader extends RootJsonReader[ApiData] {
    override def read(json: JsValue): ApiData = ApiData(
      (json \ "refs").convertTo[Seq[Ref]],
      (json \ "bookmarks").toOpt[Map[String, String]].getOrElse(Map.empty),
      (json \ "types").toOpt[Map[String, String]].getOrElse(Map.empty),
      (json \ "tags").toOpt[Seq[String]].getOrElse(Nil),
      (json \ "types").toOpt[Map[String, Form]].getOrElse(Map.empty),
      (
        (json \ "oauth_initiate").convertTo[String],
        (json \ "oauth_token").convertTo[String]
        ),
      (json \ "experiments").toOpt[Experiments].getOrElse(Experiments(Nil, Nil))
    )
  }

}


