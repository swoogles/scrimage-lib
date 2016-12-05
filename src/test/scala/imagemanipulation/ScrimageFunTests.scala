package imagemanipulation

import org.scalatest.FlatSpec

class ScrimageFunTests extends FlatSpec {

  "scrimage" should "show all the steps in a fold summation" in {
    Transformations.foldSummationImage()
  }

  "scrimage" should "create phone number fold demo image" in {
    Transformations.phoneNumbers()
  }

  "scrimage" should "draw a Map image" in {
    Transformations.drawMapImage()
  }


}
