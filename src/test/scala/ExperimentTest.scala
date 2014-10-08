package io.prismic

import org.specs2.mutable._
import play.api.libs.json._

class ExperimentTest extends Specification {

  val experimentsJson = """{
    "draft": [
        {
            "id": "xxxxxxxxxxoGelsX",
            "name": "Exp 2",
            "variations": [
                {
                    "id": "VDUBBawGAKoGelsZ",
                    "label": "Base",
                    "ref": "VDUBBawGALAGelsa"
                },
                {
                    "id": "VDUE-awGALAGemME",
                    "label": "var 1",
                    "ref": "VDUUmHIKAZQKk9uq"
                }
            ]
        }
    ],
    "running": [
        {
            "googleId": "_UQtin7EQAOH5M34RQq6Dg",
            "id": "VDUBBawGAKoGelsX",
            "name": "Exp 1",
            "variations": [
                {
                    "id": "VDUBBawGAKoGelsZ",
                    "label": "Base",
                    "ref": "VDUBBawGALAGelsa"
                },
                {
                    "id": "VDUE-awGALAGemME",
                    "label": "var 1",
                    "ref": "VDUUmHIKAZQKk9uq"
                }
            ]
        }
    ]
}"""

  "Experiments" should {
    val experiments = Json.parse(experimentsJson).as[Experiments]
    "parse correctly" in {
      experiments must beLike {
        case Experiments(Seq(exp2), Seq(exp1)) =>
          exp1.id must_== "VDUBBawGAKoGelsX"
          exp1.googleId must_== Some("_UQtin7EQAOH5M34RQq6Dg")
          exp1.name must_== "Exp 1"
          exp1.variations must beLike {
            case Seq(base, var1) =>
              base.id must_== "VDUBBawGAKoGelsZ"
              base.label must_== "Base"
              base.ref must_== "VDUBBawGALAGelsa"
          }
      }
    }
    "Pick the right variation ref for a cookie content" in {
      val f = experiments.refFromCookie _
      "empty cookie" in {
        f("") must beNone
      }
      "invalid cookie content" in {
        f("Poneys are awesome") must beNone
      }
      "actual running variation index" in {
        f("_UQtin7EQAOH5M34RQq6Dg%200") must beSome("VDUBBawGALAGelsa")
        f("_UQtin7EQAOH5M34RQq6Dg%201") must beSome("VDUUmHIKAZQKk9uq")
      }
      "index overflow" in {
        f("_UQtin7EQAOH5M34RQq6Dg%2099") must beNone
        f("_UQtin7EQAOH5M34RQq6Dg%20-1") must beNone
      }
      "unknown google experiment ID" in {
        f("notAGoodLookingId%200") must beNone
        f("notAGoodLookingId%201") must beNone
      }
    }
  }
}
