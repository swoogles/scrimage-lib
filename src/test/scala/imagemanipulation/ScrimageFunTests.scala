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

  "scrimage" should "overlay 1 image on another" in {
    import ammonite.ops._
    implicit val wd: ammonite.ops.Path = cwd / "OriginalImages"
    val img1 = (wd / "rollercoaster_ride.jpg").toIO
    val img2 = (wd / "SARAnight.jpg").toIO
    val generatedImages: ammonite.ops.Path = cwd / "GeneratedImages"
    val compositeImage  = ScrimageFun.compositeImages(img1, img2)
    import com.sksamuel.scrimage.nio.JpegWriter
    val compositeOutFile = generatedImages / "composite.jpg"
    // compositeImage.output(compositeOutFile.toIO)(JpegWriter())
  }

}
