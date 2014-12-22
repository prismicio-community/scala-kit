package io.prismic

/**
 * Builds URL specific to an application, based on a generic prismic.io document link.
 */
trait DocumentLinkResolver {
  def apply(link: Fragment.DocumentLink): String
  def apply(document: Document): String = apply(document.asDocumentLink)
}

/**
 * DocumentLinkResolver builders
 */
object DocumentLinkResolver {

  /**
   * Builds a DocumentLinkResolver
   */
  def apply(api: Api)(f: (((Fragment.DocumentLink, Option[String])) => String)) = new DocumentLinkResolver {
    def apply(link: Fragment.DocumentLink): String = f((link, api.bookmarks.find(_._2 == link.id).map(_._1)))
  }

  /**
   * Builds a DocumentLinkResolver
   */
  def apply(f: Fragment.DocumentLink => String) = new DocumentLinkResolver {
    def apply(link: Fragment.DocumentLink): String = f(link)
  }

}
