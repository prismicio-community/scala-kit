package io.prismic

import io.prismic.fragments.{AlternateLanguage, DocumentLink}
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
  lang: String,
  alternateLanguages: Seq[AlternateLanguage],
  fragments: Map[String, Fragment]) extends WithFragments {

  def slug: String = slugs.headOption.getOrElse("-")

  def isTagged(requiredTags: Seq[String]) = requiredTags.forall(tag => tags.contains(tag))

  def asDocumentLink: DocumentLink = DocumentLink(id, uid, typ, tags, slug, lang, fragments, isBroken = false)
}
