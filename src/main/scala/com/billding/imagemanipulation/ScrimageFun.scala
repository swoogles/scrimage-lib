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
    val startingTextBox = HeadLongTextListItem("801-971-9844")
    // case class TailLongTextListItem(content: String, prevItem: LongItem, value: Int = 1) extends LongItem {

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

  def phoneNumbers() = {
    val areaCodesAndStates = Map(
      "801"->"UT",
      "865"->"TN"
    )
    val phoneNumbers = List(
      "801-971-9844",
      "800-222-3333",
      "865-104-1623",
      "865-200-3273",
      "600-222-3333"
    )
    val typedPhoneList = phoneNumbers.foldLeft(HeadLongTextListItem("000-000-0000"): LongItem){
      case (acc, curNumber) => TailLongTextListItem(curNumber, acc)
    }

    val typedPhoneNumbers = phoneNumbers.zipWithIndex.map { case (number, idx) =>
      PhoneNumberListItem(
        number,
        Rect(x=200+100*idx, y=50, width=50, height=50, { g2 =>
          g2.setColor(JColor.GREEN)
          g2.setFont(imgFont)
        })
      )
    }

    val organizedNumbers = Map[String, List[String]]().withDefaultValue(List(): List[String])
    val updatedMap = (organizedNumbers
      + ("UNKNOWN" -> List())
      + ("UT" -> List("911"))
      + ("CO" -> List("100-200-3000"))
      )
    pprint.pprintln(updatedMap)

    val locationFolding = typedPhoneNumbers.foldLeft(updatedMap){ case (acc, li) =>
      val lookupRes: Option[String] = areaCodesAndStates.get(li.phoneNumber.take(3))
      val region = lookupRes.getOrElse("UNKNOWN")
      val neighboringEntries = acc(region)
      acc + (region -> (li.phoneNumber :: neighboringEntries))
    }


    val locationLookups = typedPhoneNumbers.map{ case li: PhoneNumberListItem =>
        areaCodesAndStates.get(li.phoneNumber.take(3))
    }.flatten.toSet

    pprint.pprintln(locationFolding)
  

    val img: Canvas = Image(1400, 800)
      .fit(1400, 800, Color.Black)
    val imgPath = generatedImgDir / (s"phone_folding.jpg")

    val scannedDrawables = LongItem.allDrawables(typedPhoneList)
    pprint.pprintln(scannedDrawables)

    val imgWithScannedDrawables = scannedDrawables.foldLeft(img){
      case (curImg: Canvas, li: Drawable) => curImg.draw(li)
    }

    imgWithScannedDrawables.output(imgPath.toIO)(JpegWriter())
  }
}

sealed trait CustomDrawable {
  val rect: Rect
  val value: Int
  val content: String
  val imgFont = new JFont("Sans-seriff", 1, 28)

  def text: Text =
       Text(content.toString, rect.x+15, rect.y+30, { g2 =>
         g2.setBackground(JColor.BLUE)
         g2.setFont(imgFont)
       })

}

sealed trait LongItem extends CustomDrawable {
  override val imgFont = new JFont("Sans-seriff", 1, 14)
}

object LongItem {
  def allDrawables(li: LongItem): List[Drawable] = li match {
    case head: HeadLongTextListItem => List(head.rect, head.text)
    case tail: TailLongTextListItem => List(tail.rect, tail.text) ::: allDrawables(tail.prevItem)
  }
}

case class HeadLongTextListItem(content: String, value: Int = 1) extends LongItem {
  val rect =
        Rect(x=100, y=50, width=150, height=50)
}

case class TailLongTextListItem(content: String, prevItem: LongItem, value: Int = 1) extends LongItem {
  val rect =
        Rect(x=prevItem.rect.x+200, y=50, width=150, height=50)
}


case class NumericalListItem(rect: Rect, value: Int = 1) extends CustomDrawable {
  val content = value.toString
}

case class ListItem(rect: Rect, label: Char, value: Int = 1) extends CustomDrawable {
  val content = label.toString
  override val text =
       Text(label.toString, rect.x+15, rect.y+30, { g2 =>
         g2.setColor(JColor.BLUE)
         g2.setBackground(JColor.WHITE)
         g2.setFont(imgFont)
       })
}

case class PhoneNumberListItem(phoneNumber: String, rect: Rect, value: Int = 1) extends CustomDrawable {
  val content = phoneNumber
}
