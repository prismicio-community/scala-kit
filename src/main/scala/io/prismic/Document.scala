package io.prismic

import org.joda.time.DateTime
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
    fragments: Map[String, Fragment],
    firstPublicationDate: Option[DateTime],
    lastPublicationDate: Option[DateTime]) extends WithFragments {

  def slug: String = slugs.headOption.getOrElse("-")

  def isTagged(requiredTags: Seq[String]) = requiredTags.forall(tag => tags.contains(tag))

  def asDocumentLink: DocumentLink = DocumentLink(id, uid, typ, tags, slug, fragments, isBroken = false)
}
