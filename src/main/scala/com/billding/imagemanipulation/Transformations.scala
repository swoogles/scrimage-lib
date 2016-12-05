package imagemanipulation

import com.sksamuel.scrimage.nio.JpegWriter
import com.sksamuel.scrimage.Color
import com.sksamuel.scrimage.Image
import com.sksamuel.scrimage.canvas.Canvas
import java.awt.{Color => JColor, Font => JFont, Image => JImage}

import com.sksamuel.scrimage.canvas.Canvas._
import com.sksamuel.scrimage.canvas.Font
import com.sksamuel.scrimage.canvas.drawable.Text

import com.sksamuel.scrimage.canvas.drawable.Rect
import com.sksamuel.scrimage.canvas.Drawable

object Transformations extends TextDrawing with FileSystemOperations {
  import ammonite.ops._

  def drawMapImage() = {
    val mapContents = """
    Map(
      "UNKNOWN" -> List("600-222-3333", "800-222-3333"),
      "UT" -> List("801-971-9844", "911"),
      "CO" -> List("100-200-3000"),
      "TN" -> List("865-200-3273", "865-104-1623")
    )
    """.split("\n").toList
    makeImgFromText(mapContents)
  }

  def imageGeneratingFunction( imgName: String)( imgProducer: Image => Image) = {
    val img: Canvas = Image(1400, 800)
      .fit(1400, 800, Color.Black)
    val finalImage = imgProducer(img)
    val imgPath = generatedImgDir / (s"$imgName.jpg")
    finalImage.output(imgPath.toIO)(JpegWriter())
  }

  def foldSummationImage() = imageGeneratingFunction("rectangles") { img =>
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

    val scannedDrawables = typedListsB.flatMap { tup => tup._1 +: tup._2 }

    val imgWithScannedDrawables = scannedDrawables.foldLeft(img){
      case (curImg: Image, li: CustomDrawable) => curImg.draw(li.rect).draw(li.text)
    }

    imgWithScannedDrawables
  }

  def phoneNumbers() = imageGeneratingFunction("phone_folding") { img =>
    val areaCodesAndStates = Map(
      "801"->"UT",
      "970"->"CO"
    )
    val phoneNumbers = List(
      "801-971-9844",
      "970-104-1623",
      "801-200-3273",
      "970-222-3333",
      "800-222-3333"
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

    // Functional buildup method
    val locationFolding = typedPhoneNumbers.scanLeft(organizedNumbers){ case (acc, li) =>
      val lookupRes: Option[String] = areaCodesAndStates.get(li.phoneNumber.take(3))
      val region = lookupRes.getOrElse("UNKNOWN")
      val neighboringEntries = acc(region)
      acc + (region -> (li.phoneNumber :: neighboringEntries))
    }

    val foldedLocationDrawables: List[Text] = locationFolding.zipWithIndex.flatMap { case(currentLocations, idx) =>
      val locationMap = pprint.stringify(currentLocations, width=40).split("\n").toList
      // makeImgFromText(locationMap, idx )
      makeTextDrawable(locationMap, 200, 200)
    }
    pprint.pprintln(locationFolding, width=40)

    val scannedDrawables = LongItem.allDrawables(typedPhoneList) ++ foldedLocationDrawables

    val imgWithScannedDrawables = scannedDrawables.foldLeft(img){
      case (curImg: Image, li: Drawable) => curImg.draw(li)
    }

    imgWithScannedDrawables
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
