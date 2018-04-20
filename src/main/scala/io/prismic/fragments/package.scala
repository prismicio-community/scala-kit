package io.prismic.fragments

import org.joda.time._
import spray.json._
import io.prismic._

case object Separator extends Fragment {
  def asHtml = "<hr/>"
}

sealed trait Link extends Fragment {
  def getUrl(linkResolver: DocumentLinkResolver): String
}

case class WebLink(url: String, target: Option[String] = None) extends Link {
  override def getUrl(linkResolver: DocumentLinkResolver) = url
  def asHtml(content: Option[String] = None): String = {
    val linkContent = content.getOrElse(url)
    val renderTarget = target.map { t => s""" target="$t" rel="noopener"""" }.getOrElse("")
    s"""<a href="$url"$renderTarget>$linkContent</a>"""
  }
}

case class FileLink(url: String, kind: String, size: Long, filename: String, target: Option[String] = None) extends Link {
  override def getUrl(linkResolver: DocumentLinkResolver) = url
  def asHtml(content: Option[String] = None): String = {
    val linkContent = content.getOrElse(filename)
    val renderTarget = target.map { t => s""" target="$t" rel="noopener"""" }.getOrElse("")
    s"""<a href="$url"$renderTarget>$linkContent</a>"""
  }
}

case class ImageLink(url: String, kind: String, size: Long, filename: String, target: Option[String] = None) extends Link {
  override def getUrl(linkResolver: DocumentLinkResolver) = url
  def asHtml(content: Option[String] = None): String = {
    val linkContent = content.getOrElse(filename)
    val renderTarget = target.map { t => s""" target="$t" rel="noopener"""" }.getOrElse("")

    val img = s"""<img src="$url" alt="$linkContent"/>"""

    target match {
      case Some(t) => s"""<a href="$url" target="$t">$img</a>"""
      case None => img
    }
  }
}

case class DocumentLink(id: String,
                        uid: Option[String],
                        typ: String,
                        tags: Seq[String],
                        slug: String,
                        lang: String,
                        fragments: Map[String, Fragment],
                        isBroken: Boolean,
                        target: Option[String] = None) extends Link with WithFragments {
  override def getUrl(linkResolver: DocumentLinkResolver) = linkResolver(this)
  override def asHtml(linkResolver: DocumentLinkResolver, content: Option[String] = None): String = {
    val linkContent = content.getOrElse(slug)
    val renderTarget = target.map { t => s""" target="$t" rel="noopener"""" }.getOrElse("")
    s"""<a href="${linkResolver(this)}"$renderTarget>$linkContent</a>"""
  }
}

case class AlternateLanguage(id: String, uid: Option[String], typ: String, lang: String)

  // ------------------

  case class Text(value: String) extends Fragment {
    def asHtml: String = s"""<span class="text">$value</span>"""
  }

  // ------------------

  case class Date(value: LocalDate) extends Fragment {
    def asText(pattern: String) = value.toString(pattern)
    def asHtml: String = s"""<time>$value</time>"""
  }

  case class Timestamp(value: DateTime) extends Fragment {
    def asText(pattern: String) = value.toString(pattern)
    def asHtml: String = s"""<time>$value</time>"""
  }

  // ------------------

  case class Number(value: Double) extends Fragment {
    def asInt = value.toInt
    def asText(pattern: String) = new java.text.DecimalFormat(pattern).format(value)
    def asHtml: String = s"""<span class="number">$value</span>"""
  }

  // ------------------

  case class Color(hex: String) extends Fragment {
    def asRGB = Color.asRGB(hex)
    def asHtml: String = s"""<span class="color">$hex</span>"""
  }

  object Color {

    private val HexColor = """#([a-fA-F0-9]{2})([a-fA-F0-9]{2})([a-fA-F0-9]{2})""".r

    def isValidColorValue(hex: String): Boolean = hex match {
      case HexColor(r, g, b) => true
      case _                 => false
    }

    def asRGB(hex: String): (Int, Int, Int) = hex match {
      case HexColor(r, g, b) => (Integer.parseInt(r, 16), Integer.parseInt(g, 16), Integer.parseInt(b, 16))
      case _                 => (0, 0, 0)
    }

  }

  // ------------------

  case class Embed(typ: String,
                   provider: Option[String],
                   url: String,
                   width: Option[Int],
                   height: Option[Int],
                   html: Option[String],
                   oembedJson: JsValue) extends Fragment {
    def asHtml(label: Option[String] = None, direction: Option[String] = None): String = {
      val attributes: Seq[(String, String)] = Seq(
        ("data-oembed", url),
        ("data-oembed-type", typ.toLowerCase)
      ) ++
        label.map(l => ("class", l)).toSeq ++
        direction.map(d => ("dir", d)).toSeq ++
        provider.map(p => ("data-oembed-provider", p.toLowerCase))
      html.map(html => s"""<div ${attributes.map{case (k, v) => s"""$k="$v""""}.mkString(" ")}>$html</div>""").getOrElse("")
    }
  }

  // ------------------

  case class GeoPoint(latitude: Double, longitude: Double) extends Fragment {
    def asHtml: String = s"""<div class="geopoint"><span class="latitude">${latitude}</span><span class="longitude">${longitude}</span></div>"""
  }

  // ------------------

  case class Image(main: Image.View, views: Map[String, Image.View] = Map.empty) extends Fragment {

    def getView(key: String): Option[Image.View] = key.toLowerCase match {
      case "main" => Some(main)
      case _      => views.get(key)
    }

    def asHtml: String = main.asHtml

  }

  object Image {

    case class View(url: String, width: Int, height: Int, alt: Option[String]) {
      def ratio = width / height
      def asHtml: String = s"""<img alt="${alt.getOrElse("")}" src="$url" width="$width" height="$height" />"""
    }

  }

  case class Group(docs: Seq[Group.Doc]) extends Fragment {

    def asHtml(linkResolver: DocumentLinkResolver): String =
      docs map (_ asHtml linkResolver) mkString "\n"
  }

  object Group {

    case class Doc(fragments: Map[String, Fragment]) extends WithFragments

  }

  case class StructuredText(blocks: Seq[StructuredText.Block]) extends Fragment {

    def getTitle: Option[StructuredText.Block.Heading] = blocks.collectFirst {
      case h: StructuredText.Block.Heading => h
    }

    def getFirstParagraph: Option[StructuredText.Block.Paragraph] = blocks.collectFirst {
      case p: StructuredText.Block.Paragraph => p
    }

    def getAllParagraphs: Seq[StructuredText.Block.Paragraph] = blocks.collect {
      case p: StructuredText.Block.Paragraph => p
    }

    def getFirstImage: Option[StructuredText.Block.Image] = blocks.collectFirst {
      case i: StructuredText.Block.Image => i
    }

    def asHtml(linkResolver: DocumentLinkResolver, htmlSerializer: HtmlSerializer = HtmlSerializer.empty): String = {
      StructuredText.asHtml(blocks, linkResolver, htmlSerializer)
    }

  }

  trait Slice {
    def sliceType: String
    def sliceLabel: Option[String]
    def asHtml(linkResolver: DocumentLinkResolver): String
  }

  case class CompositeSlice(sliceType: String, sliceLabel: Option[String], nonRepeat: Group.Doc, repeat: Group) extends Slice {

    def asHtml(linkResolver: DocumentLinkResolver): String = {
      var className = (Seq("slice") ++ sliceLabel.toSeq).mkString(" ")
      s"""<div data-slicetype="$sliceType" class="$className">
        <div class="non-repeat">
          ${nonRepeat.fragments.toSeq.map{case (key, value) => Fragment.getHtml(value, linkResolver)}}
        </div>
        ${repeat.asHtml(linkResolver)}
      </div>"""
    }
  }

  case class SimpleSlice(sliceType: String, sliceLabel: Option[String], value: Fragment) extends Slice {

    def asHtml(linkResolver: DocumentLinkResolver): String = {
      var className = (Seq("slice") ++ sliceLabel.toSeq).mkString(" ")
      s"""<div data-slicetype="$sliceType" class="$className">${Fragment.getHtml(value, linkResolver)}</div>"""
    }

  }

  case class SliceZone(slices: Seq[Slice]) extends Fragment {

    def asHtml(linkResolver: DocumentLinkResolver, htmlSerializer: HtmlSerializer = HtmlSerializer.empty): String =
      slices map (_ asHtml linkResolver) mkString "\n"

  }

// ------------------

object StructuredText {

  def asHtml(blocks: Seq[Block], linkResolver: DocumentLinkResolver, htmlSerializer: HtmlSerializer): String = {
    case class Group(htmlTag: Option[String], blocks: Seq[Block])

    val grouped: List[Group] = blocks.foldLeft(List.empty[Group]) {
      case ((group@Group(Some("ul"), _)) :: rest, block@StructuredText.Block.ListItem(_, _, false, _, _)) => group.copy(blocks = group.blocks :+ block) +: rest
      case ((group@Group(Some("ol"), _)) :: rest, block@StructuredText.Block.ListItem(_, _, true, _, _)) => group.copy(blocks = group.blocks :+ block) +: rest
      case (groups, block@StructuredText.Block.ListItem(_, _, false, _, _)) => Group(Some("ul"), Seq(block)) +: groups
      case (groups, block@StructuredText.Block.ListItem(_, _, true, _, _)) => Group(Some("ol"), Seq(block)) +: groups
      case (groups, block) => Group(None, Seq(block)) +: groups
    }.reverse

    grouped.flatMap {
      case Group(Some(tag), bcks) => s"<$tag>" +: bcks.map(block => Block.asHtml(block, linkResolver, htmlSerializer)) :+ s"</$tag>"
      case Group(None, bcks)      => bcks.map(block => Block.asHtml(block, linkResolver, htmlSerializer))
    }.mkString("\n\n")
  }

  private def asHtml(text: String, spans: Seq[Span], linkResolver: DocumentLinkResolver, serializer: HtmlSerializer): String = {

    def escape(character: String): String = {
      character.replace("<", "&lt;").replace("\n", "<br>")
    }

    def serialize(element: Element, content: String): String = {
      serializer(element, content).getOrElse {
        element match {
          case b: Block => Block.asHtml(b, linkResolver)
          case _: Span.Em => s"<em>$content</em>"
          case _: Span.Strong => s"<strong>$content</strong>"
          case Span.Hyperlink(_, _, link: DocumentLink) => link.asHtml(linkResolver, Some(content))
          case Span.Hyperlink(_, _, link: FileLink) => link.asHtml(Some(content))
          case Span.Hyperlink(_, _, link: WebLink) => link.asHtml(Some(content))
          case Span.Label(_, _, label) => s"""<span class="$label">$content</span>"""
          case _ => s"<span>$content</span>"
        }
      }
    }

    case class OpenSpan(span: Span, content: String)

    @scala.annotation.tailrec
    def step(in: Seq[(Char, Int)], spans: Seq[Span], stack: Seq[OpenSpan] = Nil, html: String = ""): String = {
      in match {
        case ((_, pos) :: tail) if stack.headOption.map(_.span.end) == Some(pos) => {
          // Need to close a tag
          val tagHtml = serialize(stack.head.span, stack.head.content)
          stack.drop(1) match {
            case Nil => step(in, spans, Nil, html + tagHtml)
            case h :: t => step(in, spans, h.copy(content = h.content + tagHtml) :: t, html)
          }
        }
        case ((_, pos) :: tail) if spans.headOption.map(_.start) == Some(pos) => {
          // Need to open a tag
          step(in, spans.drop(1), OpenSpan(spans.head, "") +: stack, html)
        }
        case (current, pos) :: tail => {
          stack match {
            case Nil =>
              // Top level
              step(tail, spans, stack, html + escape(current.toString))
            case head :: t =>
              // There is an open span, insert inside
              step(tail, spans, head.copy(content = head.content + escape(current.toString)) :: t, html)
          }
        }
        case Nil =>
          stack match {
            case Nil => html
            case head :: Nil =>
              // One last tag open, close it
              html + serialize(head.span, head.content)
            case head :: second :: tail =>
              // At least 2 tags open, close the first and continue
              step(Nil, spans, second.copy(content = second.content + serialize(head.span, head.content)) :: tail, html)
          }
      }
    }
    step(text.toList.zipWithIndex, spans.sortWith {
      case (a, b) if a.start == b.start => (a.end - a.start) > (b.end - b.start)
      case (a, b) => a.start < b.start
    })
  }

  sealed trait Element

  sealed trait Span extends Element {
    def start: Int
    def end: Int
  }

  object Span {

    case class Em(start: Int, end: Int) extends Span
    case class Strong(start: Int, end: Int) extends Span
    case class Hyperlink(start: Int, end: Int, link: Link) extends Span
    case class Label(start: Int, end: Int, label: String) extends Span

  }

  sealed trait Block extends Element {
    def label: Option[String]
    def direction: Option[String]
  }

  object Block {

    sealed trait Text extends Block {
      def text: String
      def spans: Seq[Span]
    }

    object Text {
      def unapply(t: Text): Option[(String, Seq[Span], Option[String])] = Some(t.text, t.spans, t.label)
    }

    def asHtml(block: Block, linkResolver: DocumentLinkResolver, htmlSerializer: HtmlSerializer = HtmlSerializer.empty): String = {
      val cls = (
        block.label.map(l => s""" class="$l"""").toSeq ++
          block.direction.map(d => s""" dir="$d"""").toSeq
      ).mkString(" ")
      val body = block match {
        case StructuredText.Block.Text(text, spans, _) => StructuredText.asHtml(text, spans, linkResolver, htmlSerializer)
        case _ => ""
      }
      htmlSerializer(block, body).getOrElse {
        block match {
          case StructuredText.Block.Heading(text, spans, level, _, _) => s"""<h$level$cls>$body</h$level>"""
          case StructuredText.Block.Paragraph(text, spans, _, _) => s"""<p$cls>$body</p>"""
          case StructuredText.Block.Preformatted(text, spans, _, _) => s"""<pre$cls>$body</pre>"""
          case StructuredText.Block.ListItem(text, spans, _, _, _) => s"""<li$cls>$body</li>"""
          case StructuredText.Block.Image(view, hyperlink, label, dir) => {
            val linkbody = hyperlink match {
              case Some(link: DocumentLink) => link.asHtml(linkResolver, Some(view.asHtml))
              case Some(link: WebLink) => link.asHtml(Some(view.asHtml))
              case Some(link: FileLink) => link.asHtml(Some(view.asHtml))
              case _ => view.asHtml
            }
            s"""<p${dir.map(d => s""" dir="$d"""").getOrElse("")} class="${(label.toSeq :+ "block-img").mkString(" ")}">$linkbody</p>"""
          }
          case StructuredText.Block.Embed(obj, label, direction) => obj.asHtml(label, direction)
        }
      }
    }

    case class Heading(text: String, spans: Seq[Span], level: Int, label: Option[String], direction: Option[String]) extends Text

    case class Paragraph(text: String, spans: Seq[Span], label: Option[String], direction: Option[String]) extends Text

    case class Preformatted(text: String, spans: Seq[Span], label: Option[String], direction: Option[String]) extends Text

    case class ListItem(text: String, spans: Seq[Span], ordered: Boolean, label: Option[String], direction: Option[String]) extends Text

    case class Image(view: fragments.Image.View, linkTo: Option[Link], label: Option[String], direction: Option[String]) extends Block {
      def url = view.url
      def width = view.width
      def height = view.height
    }

    case class Embed(obj: fragments.Embed, label: Option[String], direction: Option[String]) extends Block

  }

}
