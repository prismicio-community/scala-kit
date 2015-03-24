package io.prismic

import io.prismic.fragments.DocumentLink

/**
 * Builds URL specific to an application, based on a generic prismic.io document link.
 */
trait DocumentLinkResolver {
  def apply(link: DocumentLink): String
  def apply(document: Document): String = apply(document.asDocumentLink)
}

/**
 * DocumentLinkResolver builders
 */
object DocumentLinkResolver {

  /**
   * Builds a DocumentLinkResolver
   */
  def apply(api: Api)(f: (((DocumentLink, Option[String])) => String)) = new DocumentLinkResolver {
    def apply(link: DocumentLink): String = f((link, api.bookmarks.find(_._2 == link.id).map(_._1)))
  }

  /**
   * Builds a DocumentLinkResolver
   */
  def apply(f: DocumentLink => String) = new DocumentLinkResolver {
    def apply(link: DocumentLink): String = f(link)
  }

}
