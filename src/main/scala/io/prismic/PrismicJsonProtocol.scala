package io.prismic

import io.prismic.fragments._, Image.View, StructuredText.Block, StructuredText.Span._

import org.joda.time._
import org.joda.time.format.ISODateTimeFormat
import spray.json._

import PrismicJson._

import scala.util.Try

object PrismicJsonProtocol extends DefaultJsonProtocol with NullOptions {

  // Fragments

  implicit object WeblinkFormat extends RootJsonFormat[WebLink] {
    override def read(json: JsValue): WebLink = json.asJsObject.getFields("url") match {
      case Seq(JsString(url)) => WebLink(url)
      case _ => throw new DeserializationException("Expected url field")
    }
    override def write(obj: WebLink): JsValue = throw new SerializationException("Not implemented")
  }

  implicit object FileLinkFormat extends RootJsonFormat[FileLink] {
    override def read(json: JsValue): FileLink = (json \ "file").asJsObject.getFields("url", "kind", "size", "name") match {
      case Seq(JsString(url), JsString(kind), JsString(size), JsString(name)) => FileLink(url, kind, size.toLong, name)
      case _ => throw new DeserializationException("Missing field")
    }

    override def write(obj: FileLink): JsValue = throw new SerializationException("Not implemented")
  }

  implicit object ImageLinkFormat extends RootJsonFormat[ImageLink] {
    override def read(json: JsValue): ImageLink = (json \ "image").asJsObject.getFields("url", "kind", "size", "name") match {
      case Seq(JsString(url), JsString(kind), JsString(size), JsString(name)) => ImageLink(url, kind, size.toLong, name)
      case _ => throw new DeserializationException("Missing field")
    }

    override def write(obj: ImageLink): JsValue = throw new SerializationException("Not implemented")
  }

  implicit object DocumentLinkFormat extends RootJsonFormat[DocumentLink] {
    override def read(json: JsValue): DocumentLink = {
      val typ = (json \ "document" \ "type").convertTo[String]
      val fragments: Map[String, Fragment] = json \ "document" \ "data" \ typ match {
        case js:JsObject => DocumentFormat.parseFragments(js, typ)
        case _ => Map.empty
      }

      DocumentLink(
        (json \ "document" \ "id").convertTo[String],
        (json \ "document" \ "uid").toOpt[String],
        typ,
        (json \ "document" \ "tags").toOpt[Seq[String]].getOrElse(Nil),
        (json \ "document" \ "slug").convertTo[String],
        fragments,
        (json \ "isBroken").toOpt[Boolean].getOrElse(false)
      )
    }
    override def write(obj: DocumentLink): JsValue = throw new SerializationException("Not implemented")
  }

  implicit object LinkFormat extends RootJsonFormat[Link] {
    override def read(json: JsValue): Link = (json \ "type").convertTo[String] match {
      case "Link.web" => (json \ "value").convertTo[WebLink]
      case "Link.document" => (json \ "value").convertTo[DocumentLink]
      case "Link.file" => (json \ "value").convertTo[FileLink]
      case "Link.image" => (json \ "value").convertTo[ImageLink]
      case t => throw new DeserializationException(s"Unkown link type $t")
    }
    override def write(obj: Link): JsValue = throw new SerializationException("Not implemented")
  }

  implicit object DateFormat extends RootJsonFormat[Date] {
    override def read(json: JsValue): Date =
      Try(Date(LocalDate.parse(json.convertTo[String], format.DateTimeFormat.forPattern("yyyy-MM-dd")))).getOrElse {
        throw new DeserializationException("date parsing error ")
    }
    override def write(obj: Date): JsValue = throw new SerializationException("Not implemented")
  }

  implicit object TextFormat extends RootJsonFormat[Text] {
    override def read(json: JsValue): Text = Text(json.convertTo[String])
    override def write(obj: Text): JsValue = throw new SerializationException("Not implemented")
  }

  implicit object TimestampFormat extends RootJsonFormat[Timestamp] {
    override def read(json: JsValue): Timestamp = {
      val d = json.convertTo[String]
        val isoFormat = org.joda.time.format.DateTimeFormat.forPattern("yyyy-MM-dd'T'HH:mm:ssZ")
        Try(Timestamp(DateTime.parse(d, isoFormat).withZone(DateTimeZone.UTC)))
          .getOrElse(throw new DeserializationException(s"Invalid timestamp value $d"))
    }
    override def write(obj: Timestamp): JsValue = throw new SerializationException("Not implemented")
  }

  implicit object NumberFormat extends RootJsonFormat[Number] {
    override def read(json: JsValue): Number = Number(json.convertTo[Double])
    override def write(obj: Number): JsValue = throw new SerializationException("Not implemented")
  }

  implicit object ColorFormat extends RootJsonFormat[Color] {
    override def read(json: JsValue): Color = json match {
      case JsString(hex) =>
        if (Color.isValidColorValue(hex)) {
          Color(hex)
        } else {
          throw new DeserializationException(s"Invalid color value $hex")
        }
      case _ => throw new DeserializationException(s"Expected String for Color, got " + json)
    }
    override def write(obj: Color): JsValue = throw new SerializationException("Not implemented")
  }

  implicit object EmbedFormat extends RootJsonFormat[Embed] {
    override def read(json: JsValue): Embed = Embed(
      (json \ "oembed" \ "type").convertTo[String],
      (json \ "oembed" \ "provider_name").toOpt[String],
      (json \ "oembed" \ "embed_url").convertTo[String],
      (json \ "oembed" \ "width").toOpt[Int],
      (json \ "oembed" \ "height").toOpt[Int],
      (json \ "oembed" \ "html").toOpt[String],
      json \ "oembed"
    )

    override def write(obj: Embed): JsValue = throw new SerializationException("Not implemented")
  }

  implicit object GeoPointFormat extends RootJsonFormat[GeoPoint] {
    override def read(json: JsValue): GeoPoint = json.asJsObject.getFields("latitude", "longitude") match {
      case Seq(JsNumber(latitude), JsNumber(longitude)) => GeoPoint(latitude.toDouble, longitude.toDouble)
      case _ => throw new DeserializationException("Expected longitude and latitude as numbers")
    }
    override def write(obj: GeoPoint): JsValue = throw new SerializationException("Not implemented")
  }

  implicit object SpanFormat extends RootJsonFormat[StructuredText.Span] {
    override def read(json: JsValue): StructuredText.Span = json.asJsObject.getFields("type", "start", "end", "data") match {
      case Seq(JsString("strong"), JsNumber(start), JsNumber(end)) => StructuredText.Span.Strong(start.toInt, end.toInt)
      case Seq(JsString("em"), JsNumber(start), JsNumber(end)) => StructuredText.Span.Em(start.toInt, end.toInt)
      case Seq(JsString("label"), JsNumber(start), JsNumber(end), JsString(label)) => StructuredText.Span.Label(start.toInt, end.toInt, label)
      case Seq(JsString("hyperlink"), JsNumber(start), JsNumber(end), data) => data.asJsObject.getFields("type", "value") match {
        case Seq(JsString("Link.web"), link) => Hyperlink(start.toInt, end.toInt, link.convertTo[WebLink])
        case Seq(JsString("Link.document"), link) => Hyperlink(start.toInt, end.toInt, link.convertTo[DocumentLink])
        case Seq(JsString("Link.file"), link) => Hyperlink(start.toInt, end.toInt, link.convertTo[FileLink])
        case Seq(JsString("Link.image"), link) => Hyperlink(start.toInt, end.toInt, link.convertTo[ImageLink])
      }
      case Seq(JsString("label"), JsNumber(start), JsNumber(end), data) => StructuredText.Span.Label(start.toInt, end.toInt, (data \ "label").convertTo[String])
    }
    override def write(obj: StructuredText.Span): JsValue = throw new SerializationException("Not implemented")
  }

  def headingFormat(level: Int) = new RootJsonFormat[StructuredText.Block.Heading] {
    override def read(json: JsValue) = StructuredText.Block.Heading(
      (json \ "text").convertTo[String],
      (json \ "spans").toOpt[Seq[StructuredText.Span]].getOrElse(Nil),
      level,
      (json \ "label").toOpt[String],
      (json \ "direction").toOpt[String]
    )
    override def write(obj: StructuredText.Block.Heading): JsValue = throw new SerializationException("Not implemented")
  }

  def listItemFormat(ordered: Boolean) = new RootJsonFormat[StructuredText.Block.ListItem] {
    override def read(json: JsValue) = StructuredText.Block.ListItem(
      (json \ "text").convertTo[String],
      (json \ "spans").toOpt[Seq[StructuredText.Span]].getOrElse(Nil),
      ordered,
      (json \ "label").toOpt[String],
      (json \ "direction").toOpt[String]
    )
    override def write(obj: StructuredText.Block.ListItem): JsValue = throw new SerializationException("Not implemented")
  }

  implicit val paragraphFormat = jsonFormat4(StructuredText.Block.Paragraph)

  implicit val preformattedFormat = jsonFormat4(StructuredText.Block.Preformatted)

  implicit object ImageViewFormat extends RootJsonFormat[Image.View] {
    override def write(obj: View): JsValue = throw new SerializationException("Not implemented")

    override def read(json: JsValue): Image.View = Image.View(
      (json \ "url").convertTo[String],
      (json \ "dimensions" \ "width").convertTo[Int],
      (json \ "dimensions" \ "height").convertTo[Int],
      (json \ "alt").toOpt[String],
      (json \ "copyright").toOpt[String]
    )
  }

  implicit object ImageBlockFormat extends RootJsonFormat[StructuredText.Block.Image] {
    override def write(obj: Block.Image): JsValue = throw new SerializationException("Not implemented")

    override def read(json: JsValue): Block.Image =
      Block.Image(
        json.convertTo[Image.View],
        (json \ "linkTo").toOpt[Link],
        (json \ "label").toOpt[String],
        (json \ "direction").toOpt[String]
      )
  }

  implicit val imageFormat = jsonFormat2(Image.apply)

  implicit object StructuredTextEmbedFormat extends RootJsonFormat[StructuredText.Block.Embed] {
    override def read(json: JsValue) = StructuredText.Block.Embed(
      json.convertTo[Embed],
      (json \ "label").toOpt[String],
      (json \ "direction").toOpt[String]
    )
    override def write(obj: StructuredText.Block.Embed): JsValue = throw new SerializationException("Not implemented")
  }

  implicit object BlockFormat extends RootJsonFormat[StructuredText.Block] {
    override def read(json: JsValue): StructuredText.Block =
      json \ "type" match {
        case JsString("heading1") => json.convertTo[StructuredText.Block.Heading](headingFormat(1))
        case JsString("heading2") => json.convertTo[StructuredText.Block.Heading](headingFormat(2))
        case JsString("heading3") => json.convertTo[StructuredText.Block.Heading](headingFormat(3))
        case JsString("heading4") => json.convertTo[StructuredText.Block.Heading](headingFormat(4))
        case JsString("heading5") => json.convertTo[StructuredText.Block.Heading](headingFormat(5))
        case JsString("heading6") => json.convertTo[StructuredText.Block.Heading](headingFormat(6))
        case JsString("paragraph") => json.convertTo[StructuredText.Block.Paragraph]
        case JsString("preformatted") => json.convertTo[StructuredText.Block.Preformatted]
        case JsString("list-item") => json.convertTo[StructuredText.Block.ListItem](listItemFormat(ordered = false))
        case JsString("o-list-item") => json.convertTo[StructuredText.Block.ListItem](listItemFormat(ordered = true))
        case JsString("image") =>  json.convertTo[StructuredText.Block.Image]
        case JsString("embed") => json.convertTo[StructuredText.Block.Embed]
        case other => throw new DeserializationException(s"Unsupported block $json")
      }
    override def write(obj: StructuredText.Block): JsValue = throw new SerializationException("Not implemented")
  }

  implicit object StructuredTextFormat extends RootJsonFormat[StructuredText] {
    override def read(json: JsValue): StructuredText = json match {
      case JsArray(elements) => StructuredText(elements.map(_.convertTo[StructuredText.Block]))
      case _ => throw new DeserializationException("Expected JsArray")
    }
    override def write(obj: StructuredText): JsValue = throw new SerializationException("Not implemented")
  }

  implicit object FragmentFormat extends RootJsonFormat[Fragment] {
    override def read(json: JsValue): Fragment = json.asJsObject.getFields("type", "value") match {
        case Seq(JsString("Image"), value) => value.convertTo[Image]
        case Seq(JsString("Number"), value) => value.convertTo[Number]
        case Seq(JsString("Date"), value) => value.convertTo[Date]
        case Seq(JsString("Timestamp"), value) => value.convertTo[Timestamp]
        case Seq(JsString("GeoPoint"), value) => value.convertTo[GeoPoint]
        case Seq(JsString("Text"), value) => value.convertTo[Text]
        case Seq(JsString("Select"), value) => value.convertTo[Text]
        case Seq(JsString("Embed"), value) => value.convertTo[Embed]
        case Seq(JsString("Link.web"), value) => value.convertTo[WebLink]
        case Seq(JsString("Link.document"), value) => value.convertTo[DocumentLink]
        case Seq(JsString("Link.file"), value) => value.convertTo[FileLink]
        case Seq(JsString("Link.image"), value) => value.convertTo[ImageLink]
        case Seq(JsString("StructuredText"), value) => value.convertTo[StructuredText]
        case Seq(JsString("Group"), value) => value.convertTo[Group]
        case Seq(JsString("SliceZone"), value) => value.convertTo[SliceZone]
        case Seq(JsString("Color"), value) => value.convertTo[Color]
        case Seq(JsString("Separator")) => Separator
        case Seq(JsString(t), _) => throw new DeserializationException(s"Unkown fragment type: $t")
        case _ => throw new DeserializationException("Expected JsObject with type and value, got " + json)
      }

    override def write(obj: Fragment): JsValue = throw new SerializationException("Not implemented")
  }

  implicit object GroupFormat extends RootJsonFormat[Group] {
    override def read(json: JsValue): Group =
      Group(json.convertTo[Seq[Map[String, Fragment]]].map(Group.Doc))

    override def write(obj: Group): JsValue = throw new SerializationException("Not implemented")
  }

  implicit object SliceFormat extends RootJsonFormat[Slice] {
    override def read(jsValue: JsValue): Slice = {
      val json = jsValue.asJsObject
      json.getFields("slice_type", "value", "non-repeat", "repeat") match {
        case Seq(JsString(sliceType), data: JsObject) =>
          val sliceLabel = (json \ "slice_label").toOpt[String]
          val fragment = data.convertTo[Fragment]
          SimpleSlice(sliceType, sliceLabel, fragment)

        case Seq(JsString(sliceType), nonRepeat: JsObject, repeat: JsArray) =>
          val sliceLabel = (json \ "slice_label").toOpt[String]
          val nr = JsArray(nonRepeat).convertTo[Group].docs.headOption.getOrElse(Group.Doc(Map()))
          val r = repeat.convertTo[Group]
          CompositeSlice(sliceType, sliceLabel, nr, r)

        case _ => throw new DeserializationException("Expected slice_type and value")
      }
    }

    override def write(obj: Slice): JsValue = throw new SerializationException("Not implemented")
  }

  implicit object SliceZoneFormat extends RootJsonFormat[SliceZone] {
    override def read(json: JsValue) = SliceZone(json match {
      case JsArray(elements) => elements.collect {
        case jsElt if jsElt.toOpt[Slice].isDefined => jsElt.convertTo[Slice]
      }
      case _ => throw new DeserializationException("Expected JsArray")
    })

    override def write(obj: SliceZone): JsValue = throw new SerializationException("Not implemented")
  }

  implicit object DocumentFormat extends RootJsonFormat[Document] {

    override def read(jsValue: JsValue): Document = {
      val json = jsValue.asJsObject
      json.getFields("id", "href", "type", "data") match {
        case Seq(JsString(id), JsString(href), JsString(typ), data: JsObject) =>
          val uid: Option[String] = (json \ "uid").toOpt[String]
          val tags: Seq[String] = (json \ "tags").toOpt[Seq[String]].getOrElse(Nil)
          val slugs: Seq[String] = (json \ "slugs").toOpt[Seq[String]].map(decode).getOrElse(Nil)
          val fragments: JsObject = (data \ typ).asJsObject
          val firstPublicationDate: Option[DateTime] = (json \ "firstPublicationDate").toOpt[DateTime]
          val lastPublicationDate: Option[DateTime] = (json \ "lastPublicationDate").toOpt[DateTime]
          Document(
            id,
            uid,
            typ,
            href,
            tags,
            slugs,
            parseFragments(fragments, typ),
            firstPublicationDate,
            lastPublicationDate
          )
        case _ => throw new DeserializationException("Expected id, href, type and data")
      }
    }

    override def write(obj: Document): JsValue = throw new SerializationException("Not implemented")

    def parseFragments(json: JsObject, typ: String): Map[String, Fragment] = {
      val fields = json.fields.map {
        case (key, jsobj: JsObject) => jsobj.toOpt[Fragment].toList.map(fragment => (s"$typ.$key", fragment))
        case (key, jsons: JsArray) => jsons.elements.zipWithIndex.collect {
          case (json: JsObject, i) => json.toOpt[Fragment].toList.map(fragment => (s"$typ.$key[$i]", fragment))
          case (jsval, i) => Nil
        }.flatten
        case (key, jsval) => Nil
      }.flatten.toSeq
      collection.immutable.ListMap(fields:_*)
    }

    private def decode(slugs: Seq[String]) = slugs.map(java.net.URLDecoder.decode(_, "UTF-8"))

  }

  implicit val responseFormat = jsonFormat(Response,
    "results", "page", "results_per_page", "results_size",
    "total_results_size", "total_pages", "next_page", "prev_page"
  )

  // API Related classes

  implicit object DateTimeFormat extends RootJsonFormat[DateTime] {

    val formatter = ISODateTimeFormat.basicDateTimeNoMillis

    def write(obj: DateTime): JsValue = {
      JsString(formatter.print(obj))
    }

    def read(json: JsValue): DateTime = json match {
      case JsString(s) => try {
        formatter.parseDateTime(s)
      }
      catch {
        case t: Throwable => error(s)
      }
      case _ =>
        error(json.toString())
    }

    def error(v: Any): DateTime = {
      val example = formatter.print(0)
      deserializationError(f"'$v' is not a valid date value. Dates must be in compact ISO-8601 format, e.g. '$example'")
    }
  }

  implicit val fieldFormat = jsonFormat3(Field)

  implicit val formFormat = jsonFormat6(Form)

  implicit object RefFormat extends RootJsonFormat[Ref] {
    override def read(json: JsValue): Ref = Ref(
      (json \ "id").convertTo[String],
      (json \ "ref").convertTo[String],
      (json \ "label").convertTo[String],
      (json \ "isMasterRef").toOpt[Boolean].getOrElse(false),
      (json \ "scheduledAt").toOpt[DateTime]
    )
    override def write(obj: Ref): JsValue = throw new SerializationException("Not implemented")
  }

  implicit val variationFormat = jsonFormat3(Variation)

  implicit val experimentFormat = jsonFormat4(Experiment)

  implicit val experimentsFormat = jsonFormat(Experiments, "draft", "running")

  implicit object ApiDataFormat extends RootJsonFormat[ApiData] {
    override def read(json: JsValue): ApiData = ApiData(
      (json \ "refs").convertTo[Seq[Ref]],
      (json \ "bookmarks").toOpt[Map[String, String]].getOrElse(Map.empty),
      (json \ "types").toOpt[Map[String, String]].getOrElse(Map.empty),
      (json \ "tags").toOpt[Seq[String]].getOrElse(Nil),
      (json \ "forms").toOpt[Map[String, Form]].getOrElse(Map.empty),
      (
        (json \ "oauth_initiate").convertTo[String],
        (json \ "oauth_token").convertTo[String]
        ),
      (json \ "experiments").toOpt[Experiments].getOrElse(Experiments(Nil, Nil))
    )
    override def write(obj: ApiData): JsValue = throw new SerializationException("Not implemented")
  }

}
