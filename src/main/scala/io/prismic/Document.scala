package io.prismic

import spray.json._

import io.prismic.fragments.{ DocumentLink, StructuredText },
  StructuredText.Span.Hyperlink

/**
 * A prismic.io document
 */
case class Document(
    id: String,
    uid: Option[String],
    typ: String,
    href: String,
    tags: Seq[String],
    slugs: Seq[String],
    fragments: Map[String, Fragment]) extends WithFragments {

  def slug: String = slugs.headOption.getOrElse("-")

  def isTagged(requiredTags: Seq[String]) = requiredTags.forall(tag => tags.contains(tag))

  def asDocumentLink: DocumentLink = DocumentLink(id, uid, typ, tags, slug, fragments, isBroken = false)
}

private[prismic] object Document {

/*
  def fragmentsReader(typ: String) = Reads[Map[String, Fragment]] {
    case JsObject(fields) => JsSuccess(collection.immutable.ListMap(fields.map {
      case (key, json: JsObject) => parse(json).toList.map(fragment => (s"$typ.$key", fragment))
      case (key, jsons: JsArray) => jsons.value.zipWithIndex.collect {
        case (json: JsObject, i) => parse(json).toList.map(fragment => (s"$typ.$key[$i]", fragment))
        case _ => Nil
      }.flatten
      case _ => Nil
    }.flatten:_*))
    case _ => JsError("JsObject expected")
  }

  implicit def reader: Reads[Document] = (
    (__ \ "id").read[String] and
    (__ \ "uid").readNullable[String] and
    (__ \ "href").read[String] and
    (__ \ "tags").read[Seq[String]] and
    (__ \ "slugs").read[Seq[String]].map(decode) and
    (__ \ "type").read[String].flatMap[(String, Map[String, Fragment])] { typ =>
      (__ \ "data" \ typ).read[Map[String, Fragment]](fragmentsReader(typ))
      .map(data => (typ, data))
    }
  )((id, uid, href, tags, slugs, typAndData) => Document(id, uid, typAndData._1, href, tags, slugs, typAndData._2))
*/
}
