package io.prismic

import io.prismic.fragments._, StructuredText.Span.Hyperlink

trait WithFragments {

  def fragments: Map[String, Fragment]

  private val IndexedKey = """^([^\[]+)(\[\d+\])?$""".r

  /**
   * Access any fragment by name
   */
  def get(field: String): Option[Fragment] = fragments.get(field).orElse(getAll(field).headOption)

  def linkedDocuments: Iterable[DocumentLink] = fragments.flatMap {
    case (_, link: DocumentLink) => Seq(link)
    case (_, text: StructuredText) => text.blocks.flatMap {
      case textBlock: StructuredText.Block.Text => textBlock.spans.flatMap {
        case Hyperlink(_, _, link:DocumentLink) => Some(link)
        case _ => None
      }
      case _ => Nil
    }
    case (_, group: Group) => group.docs.flatMap(_.linkedDocuments)
    case _ => Nil
  }

  /**
   * Access any fragment sequence by name
   */
  def getAll(field: String): Seq[Fragment] = fragments.collect {
    case (IndexedKey(key, _), fragment) if key == field => fragment
  }.toSeq

  def getLink(field: String): Option[Link] = get(field).flatMap {
    case a: WebLink      => Some(a)
    case a: FileLink    => Some(a)
    case a: DocumentLink => Some(a)
    case a: ImageLink    => Some(a)
    case _               => None
  }

  def getImage(field: String): Option[Image] = get(field).flatMap {
    case a: Image          => Some(a)
    case a: StructuredText => a.blocks.collectFirst { case b: StructuredText.Block.Image => b.view }.map(v => Image(v))
    case _                          => None
  }

  def getAllImages(field: String): Seq[Image] = getAll(field).flatMap {
    case a: Image          => Seq(a)
    case a: StructuredText => a.blocks.collect { case b: StructuredText.Block.Image => b.view }.map(v => Image(v))
    case _                          => Nil
  }

  def getImage(field: String, view: String): Option[Image.View] = get(field).flatMap {
    case a: Image => a.getView(view)
    case a: StructuredText if view == "main" => getImage(field).map(_.main)
    case _ => None
  }

  def getAllImages(field: String, view: String): Seq[Image.View] = getAll(field).flatMap {
    case a: Image => a.getView(view).toSeq
    case a: StructuredText if view == "main" => getAllImages(field).map(_.main)
    case _ => Nil
  }

  def getStructuredText(field: String): Option[StructuredText] = get(field).flatMap {
    case a: StructuredText => Some(a)
    case _                          => None
  }

  def getHtml(field: String, linkResolver: DocumentLinkResolver): Option[String] = get(field).map { f => Fragment.getHtml(f, linkResolver) }

  def getText(field: String): Option[String] = get(field).flatMap {
    case a: StructuredText => Some(a.blocks.collect { case b: StructuredText.Block.Text => b.text }.mkString("\n")).filterNot(_.isEmpty)
    case a: Number         => Some(a.value.toString)
    case a: Color          => Some(a.hex)
    case a: Text           => Some(a.value).filterNot(_.isEmpty)
    case a: Date           => Some(a.value.toString)
    case _                          => None
  }

  def getColor(field: String): Option[Color] = get(field).flatMap {
    case c: Color => Some(c)
    case _                 => None
  }

  def getNumber(field: String): Option[Number] = get(field).flatMap {
    case n: Number => Some(n)
    case _                  => None
  }

  def getDate(field: String): Option[Date] = get(field).flatMap {
    case d: Date => Some(d)
    case _                => None
  }

  def getDate(field: String, pattern: String): Option[String] = get(field).flatMap {
    case d: Date => Some(d.asText(pattern))
    case _                => None
  }

  def getTimestamp(field: String): Option[Timestamp] = get(field).flatMap {
    case t: Timestamp => Some(t)
    case _ => None
  }

  def getEmbed(field: String): Option[Embed] = get(field).flatMap {
    case e: Embed => Some(e)
    case _ => None
  }

  def getGeoPoint(field: String): Option[GeoPoint] = get(field).flatMap {
    case gp: GeoPoint => Some(gp)
    case _                => None
  }

  def getNumber(field: String, pattern: String): Option[String] = getNumber(field).map(_.asText(pattern))

  def getBoolean(field: String): Boolean = get(field).flatMap {
    case a: Text => Option(a.value.toLowerCase).collect {
      case "yes"  => true
      case "true" => true
    }
    case _ => None
  }.getOrElse(false)

  def getGroup(field: String): Option[Group] = get(field).flatMap {
    case g: Group => Some(g)
    case _                 => None
  }

  def getSliceZone(field: String): Option[SliceZone] = get(field).flatMap {
    case s: SliceZone => Some(s)
    case _ => None
  }

  def asHtml(linkResolver: DocumentLinkResolver, content: Option[String] = None): String = fragments.keys.map { field =>
    s"""<section data-field="$field">${getHtml(field, linkResolver).getOrElse("")}</section>"""
  }.mkString("\n")

}
