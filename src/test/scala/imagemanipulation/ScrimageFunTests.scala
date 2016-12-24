package imagemanipulation

import org.scalatest.FlatSpec

class ScrimageFunTests extends FlatSpec {
  import ammonite.ops.cwd
  val wd: ammonite.ops.Path = cwd / "TransformationImages"

  val transformations = new Transformations(cwd)

  "scrimage" should "show all the steps in a fold summation" in {
    transformations.foldSummationImage()
  }

  "scrimage" should "create phone number fold demo image" in {
    transformations.phoneNumbersMultiStageNonSubclass()
  }

  "scrimage" should "create user->devices fold image" in {
    transformations.devicesForUsers()
  }

  "scrimage" should "draw tomatoes without subclasses" in {
    val compositeImage  = transformations.againWithoutSubclassingRectUpdated()
  }

}
