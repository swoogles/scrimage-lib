package imagemanipulation

import org.scalatest.FlatSpec

class ScrimageFunTests extends FlatSpec {

  // "scrimage" should "show all the steps in a fold summation" in {
  //   Transformations.foldSummationImage()
  // }

  // "scrimage" should "create phone number fold demo image" in {
  //   Transformations.phoneNumbersMultiStage()
  // }

  // "scrimage" should "create user->devices fold image" in {
  //   Transformations.devicesForUsers()
  // }

  // "scrimage" should "overlay 1 image on another" in {
  //   import ammonite.ops._
  //   implicit val wd: ammonite.ops.Path = cwd / "OriginalImages"
  //   val img1 = (wd / "rollercoaster_ride.jpg").toIO
  //   val img2 = (wd / "SARAnight.jpg").toIO
  //   val compositeImage  = ScrimageFun.compositeImages(img1, img2)
  // }

  "scrimage" should "put an image in a box" in {
    import ammonite.ops._
    implicit val wd: ammonite.ops.Path = cwd / "OriginalImages"
    val img1 = (wd / "rollercoaster_ride.jpg").toIO
    val compositeImage  = ScrimageFun.putImgInBox(img1)
  }

  "scrimage" should "draw a list of img items" in {
    import ammonite.ops._
    implicit val wd: ammonite.ops.Path = cwd / "TransformationImages"
    val img1 = (wd / "single_seed.png").toIO
    
    val compositeImage  = Transformations.mapOverImages(img1)
  }

}
