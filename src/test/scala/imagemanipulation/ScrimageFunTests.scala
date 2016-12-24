package imagemanipulation

import org.scalatest.FlatSpec

class ScrimageFunTests extends FlatSpec {
  import ammonite.ops._
  implicit val wd: ammonite.ops.Path = cwd / "TransformationImages"

  "scrimage" should "show all the steps in a fold summation" in {
    Transformations.foldSummationImage()
  }

  "scrimage" should "create phone number fold demo image" in {
    Transformations.phoneNumbersMultiStageNonSubclass()
  }

  "scrimage" should "create user->devices fold image" in {
    Transformations.devicesForUsers()
  }

  "scrimage" should "draw tomatoes without subclasses" in {
    val compositeImage  = Transformations.againWithoutSubclassingRectUpdated()
  }

}
