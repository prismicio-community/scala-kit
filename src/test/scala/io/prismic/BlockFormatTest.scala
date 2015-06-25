package io.prismic

import io.prismic.PrismicJsonProtocol.BlockFormat
import io.prismic.fragments.StructuredText.Block
import org.specs2.mutable.Specification
    import spray.json._

import scala.util.Try

class BlockFormatTest extends Specification { override def is = s2"""

 Block parsing
   for heading 4                                         ${parseHeading(4)}
   for heading 5                                         ${parseHeading(5)}
   for heading 6                                         ${parseHeading(6)}
                                                                 """


  def parseHeading(level:Int) = Try{
    val json = s"""{"type":"heading$level","text":"Titre 4"}""".parseJson
    BlockFormat.read(json)
  }map(_=> true) getOrElse(false)

}
