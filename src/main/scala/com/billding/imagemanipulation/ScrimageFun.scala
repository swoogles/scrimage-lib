package imagemanipulation

import com.sksamuel.scrimage.ScaleMethod.FastScale
import com.sksamuel.scrimage.filter.TwirlFilter
import com.sksamuel.scrimage.filter.EdgeFilter
import com.sksamuel.scrimage.filter.SnowFilter
import com.sksamuel.scrimage.filter.OilFilter
import com.sksamuel.scrimage.nio.JpegWriter
import com.sksamuel.scrimage.Color
import com.sksamuel.scrimage.Image
import com.sksamuel.scrimage.canvas.Canvas
import java.awt.{Color => JColor, Font => JFont, Image => JImage}

import com.sksamuel.scrimage.filter.{RippleFilter, RippleType}
import com.sksamuel.scrimage.canvas.Canvas._
import com.sksamuel.scrimage.canvas.Font
import com.sksamuel.scrimage.canvas.drawable.Text
import com.sksamuel.scrimage.Tag
import com.sksamuel.scrimage.Directory

// Needed for SnowFilter for some weird reason.
import scala.concurrent.ExecutionContext.Implicits.global

import pprint.Config

trait TextDrawing extends FileSystemOperations {
  implicit val pprintConfig = Config()

  val imgFont = new JFont("Sans-seriff", 1, 28)
  def makeTextDrawable(content: List[String]): List[Text] = {
    makeTextDrawable(content, 20, 30)
  }

  def makeTextDrawable(content: List[String], x: Int, y: Int): List[Text] = {
    content.zipWithIndex.map { case (lineContent, lineIdx) =>
      Text(lineContent, x, y + (lineIdx * 30), { g2 =>
        g2.setBackground(JColor.WHITE)
        g2.setFont(imgFont)
      })
    }
  }

  def makeImgFromText(content: List[String], sequenceIdx: Int = 0): java.io.File = {
    val drawableText = makeTextDrawable(content)
    val img: Canvas = Image(1400, 800)
      .fit(1400, 800, Color.Black)
      .pad(100, Color.Black)
    val imgPath = generatedImgDir / (s"commit_${sequenceIdx}_.jpg")
    val imgWithText: Canvas = drawableText.foldLeft(img){
      case (curImg: Canvas, nextText: Text) => curImg.draw(nextText)
    }
    imgWithText.output(imgPath.toIO)(JpegWriter())
  }

}

object ScrimageFun extends TextDrawing with FileSystemOperations {
  val imgTextNew =
    Text("Happy Holidays!", 200, 800-20, { g2 =>
      g2.setBackground(JColor.WHITE)
      g2.setFont(imgFont)
    })
  
  def compositeImages(file1: java.io.File, file2: java.io.File) = {
    import com.sksamuel.scrimage.composite.OverlayComposite
    val image1 = Image.fromFile(file1)
    val image2 = Image.fromFile(file2)
    val composed = image2.composite(new OverlayComposite(.8), image1)
    import ammonite.ops._
    val generatedImages: ammonite.ops.Path = cwd / "GeneratedImages"
    val compositeOutFile = generatedImages / "composite.jpg"
    composed.output(compositeOutFile.toIO)(JpegWriter())
  }

  def putImgInBox(file1: java.io.File) = {
    val img: Canvas = Image(1400, 800)
      .fit(1400, 800, Color.Black)

    import com.sksamuel.scrimage.composite.OverlayComposite
    import com.sksamuel.scrimage.composite._
    val image1 = Image.fromFile(file1)
      .scaleTo(50,50, FastScale)
    val composed =
      // img.composite(new AverageComposite(.1), image1)
    img.overlay(image1, 300, 300)
      // image1.composite(new OverlayComposite(1.0), img)
    import ammonite.ops._
    val generatedImages: ammonite.ops.Path = cwd / "GeneratedImages"
    val compositeOutFile = generatedImages / "composite.jpg"
    composed.output(compositeOutFile.toIO)(JpegWriter())
  }




  val borderFunc: (Image=>Image) = (imgInner) => {
    val totalBorderSize = 150
    val borderStripeSize = 10
    Range(0, totalBorderSize / borderStripeSize).foldLeft(imgInner) { (curImg, idx) =>
      val borderStripeColor =
        if ( idx % 2 == 0) Color(200, 50, 50)
        else Color.Black

      curImg.pad(borderStripeSize, borderStripeColor)
    }
  }

  val rippleAmplitude = 30f
  val rippleWaveLength = 50f



  def makeArtGallery() = {
    for (imgPath <- copyFreshImages_!(originalImgDir, manipulatedImgDir);
         img = Image.fromFile(imgPath.toIO)
           .filter(OilFilter(5))
           .pad(100, Color(200, 50, 50))
           .pad(100, Color.Black)
           .pad(100, Color(200, 50, 50))

    ) { img.output(imgPath.toIO)(JpegWriter()) }
  }

  def downScaleImages() = {

    for (imgPath <- copyFreshImages_!(originalImgDir, manipulatedImgDir);
         img = Image.fromFile(imgPath.toIO)
           .fit(1000, 800, Color.Black)
    ) { img.output(imgPath.toIO)(JpegWriter())}
  }

  def crazyFrame() = {

    for (imgPath <- copyFreshImages_!(originalImgDir, manipulatedImgDir);
         img =
           borderFunc(Image.fromFile(imgPath.toIO)
           .fit(1000, 800, Color.Black))
    ) { img.output(imgPath.toIO)(JpegWriter())}
  }


  def makeGreetingCards() = {
    for (imgPath <- copyFreshImages_!(originalImgDir, manipulatedImgDir);
         img = Image.fromFile(imgPath.toIO)
           .filter(SnowFilter())
           .fit(800, 600, Color.Black)
           .pad(100, Color.Black)
           .draw(imgTextNew)
    ) { img.output(imgPath.toIO)(JpegWriter())}
  }

  def makeImgsFromHistory(history: List[List[String]]) =
    for ((curRevision, idx) <- history.zipWithIndex) {
      makeImgFromText(curRevision, idx)
    }

}

