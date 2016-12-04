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

import com.sksamuel.scrimage.canvas.drawable.Rect
import com.sksamuel.scrimage.canvas.Drawable

import pprint.Config

/**
  * Created by bfrasure on 11/15/16.
  */
object ScrimageFun {
  import ammonite.ops._

  implicit val pprintConfig = Config()

  val originalImgDir = ammonite.ops.pwd / 'OriginalImages
  val listed = ls! originalImgDir
  val manipulatedImgDir = cwd / 'ManipulatedImages
  val generatedImgDir = cwd / 'GeneratedImages

  def copyFreshImages_!(srcDir: Path, targetDir: Path): LsSeq = {
    val oldImages = ls! targetDir
    oldImages.map{ rm! _ }
    mkdir! targetDir
    for ( file <- (ls! srcDir) ) { cp.into(file, targetDir)  }
    ls! targetDir
  }

  def generateFreshImages_!(targetDir: Path) = {
    mkdir! targetDir
    val oldImages = ls! targetDir
    oldImages.map{ rm! _ }
//    for ( file <- (ls! srcDir) ) { cp.into(file, targetDir)  }
//    ls! targetDir
  }

  val imgFont = new JFont("Sans-seriff", 1, 28)

  val imgTextNew =
    Text("Happy Holidays!", 200, 800-20, { g2 =>
      g2.setBackground(JColor.WHITE)
      g2.setFont(imgFont)
    })

  def makeTextDrawable(content: List[String]) = {
    content.zipWithIndex.map { case (lineContent, lineIdx) =>
      Text(lineContent, 20, 30 + (lineIdx * 30), { g2 =>
        g2.setBackground(JColor.WHITE)
        g2.setFont(imgFont)
      })
    }
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

  def makeImgFromText(content: List[String], sequenceIdx: Int = 0) = {
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

  def drawSomeSquares() = {

    val typedItems = Range(1, 6).map { curIdx =>
      NumericalListItem(
        Rect(x=200+100*curIdx, y=50, width=50, height=50, { g2 =>
          g2.setColor(JColor.GREEN)
          g2.setFont(imgFont)
        }) ,
        curIdx
      )
    }


     val accumulator = NumericalListItem(Rect(x=50, y=50, width=50, height=50), 0)

     val typedListsB = typedItems.scanLeft((accumulator, typedItems)){
       case ((acc, remainingItems), nextItem) => 
         (
           acc
             .copy(
             rect=acc.rect.copy(
               y=acc.rect.y+100
             ),
             value=acc.value+nextItem.value
           ), 
           remainingItems.tail.map(li=>
               li.copy(
                 rect=li.rect.copy(
                   y=li.rect.y+100
                 )
               )
             )
           )
     }

     val typedLists = typedItems.scanLeft((0, typedItems)){
       case ((curVal, remainingItems), nextItem) => 
         (
           curVal+nextItem.value, 
           remainingItems.tail.map(li=>
               li.copy(
                 rect=li.rect.copy(
                   y=li.rect.y+100
                 )
               )
             )
           )
     }


    pprint.pprintln(typedLists)

    val img: Canvas = Image(1400, 800)
      .fit(1400, 800, Color.Black)
    val imgPath = generatedImgDir / (s"rectangles.jpg")

    val scannedDrawables = typedListsB.flatMap { tup => tup._1 +: tup._2 }
    pprint.pprintln(scannedDrawables)

    val imgWithScannedDrawables = scannedDrawables.foldLeft(img){
      case (curImg: Canvas, li: CustomDrawable) => curImg.draw(li.rect).draw(li.text)
    }

    imgWithScannedDrawables.output(imgPath.toIO)(JpegWriter())
  }
}

sealed trait CustomDrawable {
  val rect: Rect
  val value: Int
  val text: Text

}

case class NumericalListItem(rect: Rect, value: Int = 1) extends CustomDrawable {
  val imgFont = new JFont("Sans-seriff", 1, 28)
  val text =
       Text(value.toString, rect.x+15, rect.y+30, { g2 =>
         g2.setBackground(JColor.BLUE)
         g2.setFont(imgFont)
       })
}

case class ListItem(rect: Rect, label: Char, value: Int = 1) extends CustomDrawable {
  val imgFont = new JFont("Sans-seriff", 1, 28)
  val text =
       Text(label.toString, rect.x+15, rect.y+30, { g2 =>
         g2.setColor(JColor.BLUE)
         g2.setBackground(JColor.WHITE)
         g2.setFont(imgFont)
       })
}
