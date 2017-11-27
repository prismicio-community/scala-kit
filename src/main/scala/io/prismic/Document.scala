package io.prismic

import spray.json._
import io.prismic.fragments.{DocumentLink, StructuredText}
import StructuredText.Span.Hyperlink
import org.joda.time.DateTime

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
    firstPublicationDate: Option[DateTime],
    lastPublicationDate: Option[DateTime],
    fragments: Map[String, Fragment]) extends WithFragments {

  def slug: String = slugs.headOption.getOrElse("-")

  def isTagged(requiredTags: Seq[String]) = requiredTags.forall(tag => tags.contains(tag))

  def asDocumentLink: DocumentLink = DocumentLink(id, uid, typ, tags, slug, fragments, isBroken = false)
}
