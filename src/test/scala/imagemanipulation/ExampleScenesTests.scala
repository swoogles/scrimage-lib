package imagemanipulation

import org.scalatest.FlatSpec

class ExampleScenesTests extends FlatSpec {
  import ammonite.ops.cwd
  val wd: ammonite.ops.Path = cwd / "TransformationImages"

  val scenes = new ExampleScenes(cwd)

  "The summation scene" should "produce good images" in {
    scenes.foldSummationImage().steps
  }

  "The phone numbers scene" should "produce good images" in {
    scenes.phoneNumbersMultiStageNonSubclass()
  }

  "scrimage" should "create user->devices fold image" in {
    scenes.devicesForUsers()
  }

  "scrimage" should "draw tomatoes without subclasses" in {
    val compositeImage  = scenes.tomatos()
  }


}
