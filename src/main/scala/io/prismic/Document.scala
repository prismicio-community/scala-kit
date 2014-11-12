package io.prismic

import io.prismic.Fragment.DocumentLink
import play.api.libs.functional.syntax._
import play.api.libs.json._

/**
 * A reference to an other Prismic document, used in "related documents"
 * @param id the Prismic identifier
 * @param slug optional slug of the document
 * @param typ document type
 * @param tags list of tags of the document from Prismic
 */
case class LinkedDocument(id: String, slug: Option[String], typ: String, tags: Seq[String])

private[prismic] object LinkedDocument {

  implicit val reader: Reads[LinkedDocument] = (
    (__ \ "id").read[String] and
    (__ \ "slug").readNullable[String] and
    (__ \ "type").read[String] and
    (__ \ "tags").read[Seq[String]]
  )(LinkedDocument.apply _)
}

/**
 * A prismic.io document
 */
case class Document(
    id: String,
    typ: String,
    href: String,
    tags: Seq[String],
    slugs: Seq[String],
    linkedDocuments: List[LinkedDocument],
    fragments: Map[String, Fragment]) extends WithFragments {

  def slug: String = slugs.headOption.getOrElse("-")

  def isTagged(requiredTags: Seq[String]) = requiredTags.forall(tag => tags.contains(tag))

  def asDocumentLink: DocumentLink = Fragment.DocumentLink(id, typ, tags, slug, isBroken = false)
}

private[prismic] object Document {

  def parse(jsvalue: JsObject): Option[Fragment] = {
    (jsvalue \ "type").asOpt[String].flatMap {

      case "Image"          => Some(Fragment.Image.reader.map(identity[Fragment]))
      case "Color"          => Some(Fragment.Color.reader.map(identity[Fragment]))
      case "Number"         => Some(Fragment.Number.reader.map(identity[Fragment]))
      case "Date"           => Some(Fragment.Date.reader.map(identity[Fragment]))
      case "Timestamp"      => Some(Fragment.Timestamp.reader.map(identity[Fragment]))
      case "GeoPoint"       => Some(Fragment.GeoPoint.reader.map(identity[Fragment]))
      case "Text"           => Some(Fragment.Text.reader.map(identity[Fragment]))
      case "Select"         => Some(Fragment.Text.reader.map(identity[Fragment]))
      case "Embed"          => Some(Fragment.Embed.reader.map(identity[Fragment]))
      case "Link.web"       => Some(Fragment.WebLink.reader.map(identity[Fragment]))
      case "Link.document"  => Some(Fragment.DocumentLink.reader.map(identity[Fragment]))
      case "Link.file"      => Some(Fragment.MediaLink.reader.map(identity[Fragment]))
      case "StructuredText" => Some(Fragment.StructuredText.reader.map(identity[Fragment]))
      case "Group"          => Some(Fragment.Group.reader.map(identity[Fragment]))

      case t                => None
    }.flatMap(_.reads(jsvalue \ "value").asOpt)
  }

  implicit def reader = (
    (__ \ "id").read[String] and
    (__ \ "href").read[String] and
    (__ \ "tags").read[Seq[String]] and
    (__ \ "slugs").read[Seq[String]] and
    (__ \ "linked_documents").readNullable[List[LinkedDocument]].map(_.getOrElse(Nil)) and
    (__ \ "type").read[String].flatMap[(String, Map[String, Fragment])] { typ =>
      (__ \ "data" \ typ).read[JsObject].map { data =>
        collection.immutable.ListMap(
          data.fields.map {
            case (key, json: JsObject) => parse(json).toList.map(fragment => (s"$typ.$key", fragment))
            case (key, jsons: JsArray) => jsons.value.zipWithIndex.collect {
              case (json: JsObject, i) => parse(json).toList.map(fragment => (s"$typ.$key[$i]", fragment))
              case _                   => Nil
            }.flatten
            case _ => Nil
          }.flatten: _*
        )
      }.map(data => (typ, data))
    }
  )((id, href, tags, slugs, linkedDocuments, typAndData) => Document(id, typAndData._1, href, tags, slugs, linkedDocuments, typAndData._2))

}