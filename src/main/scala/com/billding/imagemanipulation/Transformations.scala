package imagemanipulation

import com.sksamuel.scrimage.nio.JpegWriter
import com.sksamuel.scrimage.Color
import com.sksamuel.scrimage.Image
import java.awt.{Color => JColor}

import com.sksamuel.scrimage.canvas.Canvas._
import com.sksamuel.scrimage.canvas.Font

import com.sksamuel.scrimage.canvas.drawable.Rect
import com.sksamuel.scrimage.canvas.Drawable

object Transformations extends TextDrawing with FileSystemOperations {
  import ammonite.ops._

  val blankImg = Image(1400, 800)
      .fit(1400, 800, Color.Black)


  def imageGeneratingFunction( imgName: String)( imgProducer: Image => Image) = {
    val finalImage = imgProducer(blankImg)
    val imgPath = generatedImgDir / (s"$imgName.jpg")
    finalImage.output(imgPath.toIO)(JpegWriter())
  }

  def multiImageGeneratingFunction( imgName: String)( imgProducer: Image => List[Image]) = {
    val finalImages = imgProducer(blankImg)

    finalImages.zipWithIndex.foreach { case(finalImage, idx) =>
      val numberedImgName = s"${imgName}_$idx"
      imageGeneratingFunction(numberedImgName){ (blankImg)=>finalImage }
    }
  }


  def foldSummationImage() = multiImageGeneratingFunction("rectangles") { img =>

    val typedItems = Range(1, 6).map { curIdx =>
      NumericalListItem(
        Rect(x=200+100*curIdx, y=50, width=50, height=50, { g2 =>
          g2.setColor(JColor.GREEN)
          g2.setFont(imgFont)
        }) ,
        curIdx
      )
    }.toList


     val accumulator = NumericalListItem(Rect(x=50, y=50, width=50, height=50), 0)

     val foldingSummation = typedItems.scanLeft((accumulator, typedItems)){
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

    val stagedDrawables: List[List[CustomDrawable]] = foldingSummation.map { tup => tup._1 +: tup._2 }
    stagedDrawables.map { currentDrawables =>
      currentDrawables.foldLeft(img){
        case (curImg: Image, li: CustomDrawable) => curImg.draw(li.rect).draw(li.text)
      }
    }

  }

  def phoneNumbersMultiStage() = multiImageGeneratingFunction("phone_folding") { img =>
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
    val typedPhoneNumbers = phoneNumbers.zipWithIndex.map { case (number, idx) =>
      PhoneNumberListItem(
        number,
        Rect(x=200+200*idx, y=50, width=150, height=50, { g2 =>
          g2.setColor(JColor.GREEN)
          g2.setFont(imgFont)
        })
      )
    }

    val organizedNumbers = Map[String, List[String]]().withDefaultValue(Nil)

    val locationFoldingWithRemaining = typedPhoneNumbers.scanLeft((organizedNumbers, typedPhoneNumbers)){ case ((sortedNumbers, remainingNumbers), li) =>
      val region = areaCodesAndStates.get(li.phoneNumber.take(3)).getOrElse("UNKNOWN")
      val neighboringEntries = sortedNumbers(region)
      (sortedNumbers + (region -> (li.phoneNumber :: neighboringEntries)), remainingNumbers.tail)
    }

    val foldedLocationDrawablesWithRemaining: List[List[Drawable]] = locationFoldingWithRemaining.zipWithIndex.map { case((currentLocations, remainingNumbers), idx) =>
      val locationMap = pprint.stringify(currentLocations, width=40).split("\n").toList
      remainingNumbers.map(_.text)  ::: remainingNumbers.map(_.rect) ::: makeTextDrawable(locationMap, 200, 200)
    }

    foldedLocationDrawablesWithRemaining.map { currentDrawables =>
      currentDrawables.foldLeft(img){
        case (curImg: Image, li: Drawable) => curImg.draw(li)
      }
    }
  }
}

