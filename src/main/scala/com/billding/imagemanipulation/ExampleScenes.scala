package imagemanipulation

import java.io.File
import java.net.URL

import com.sksamuel.scrimage.nio.JpegWriter
import com.sksamuel.scrimage.Color
import com.sksamuel.scrimage.Image

import ammonite.ops.Path

import Rows._
import Columns._


class ExampleScenes(basePath: Path) {
  val transformations = new Transformations(basePath)

  val IMG_HEIGHT = 500
  val IMG_WIDTH = 800

  val boxes = new ScaledBoxes(IMG_WIDTH, IMG_HEIGHT)

  def foldSummationImage() = transformations.multiStageImagesClass("rectangles") { img =>

    val typedItems = 
    CustomDrawable.spaceRowClassRectUpdated(
      List.fill(5) {
        CustomDrawable(
          1,
          boxes.smallRectangleAt(COL_3, ROW_1)
        )
      }
    )

    
     val accumulator = CustomDrawable(0, boxes.smallRectangleAt(COL_1, ROW_1))

     val foldingSummation = typedItems.scanLeft((accumulator, typedItems)){
       case ((acc: CustomDrawable, remainingItems: List[CustomDrawable]), nextItem: CustomDrawable) => {

         // TODO Figure out how to avoid copying value & content here.
         // It should be possible to use a single value.
         val newValue = acc.value+nextItem.value
         (
           acc
             .onNextRow
             .copy( value=newValue, content=newValue.toString), 
           remainingItems.tail.map(_.onNextRow)
         )
       }
     }

    foldingSummation.map { tup => tup._1 +: tup._2 }
  }

  def phoneNumbersMultiStageNonSubclass() = transformations.multiStageImagesClass("phone_folding_non_subclassed") { img =>
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

    val typedPhoneNumbersNew = 
      CustomDrawable.spaceRowClassRectUpdated (
        phoneNumbers.map { number =>
          CustomDrawable(boxes.wideRectangleAt(COL_1, ROW_1), StandaloneDrawing.pprintDrawingWithoutBox, number)
        }
      )

    val organizedNumbers = Map[String, List[String]]().withDefaultValue(Nil)

    val locationFoldingWithRemainingClassed: List[(Map[String,List[String]], List[CustomDrawable])] =
      typedPhoneNumbersNew.scanLeft((organizedNumbers, typedPhoneNumbersNew)){ case ((sortedNumbers, remainingNumbers), li) =>
        val region = areaCodesAndStates.get(li.content.take(3)).getOrElse("UNKNOWN")
        val neighboringEntries = sortedNumbers(region)
        (sortedNumbers + (region -> (li.content :: neighboringEntries)), remainingNumbers.tail)
      }


    locationFoldingWithRemainingClassed.map { case(currentLocations, remainingNumbers) =>
      import com.sksamuel.scrimage.canvas.drawable.Rect
      val rect = boxes.smallRectangleAt(COL_1, ROW_6)
      val pprintedContent  = pprint.stringify(currentLocations, width=40) // TODO Handle with clean function. Don't pprint here.
      val textualDataStructure = CustomDrawable(rect, StandaloneDrawing.pprintDrawingWithoutBox, pprintedContent)
      textualDataStructure :: remainingNumbers
    }

  }

}
