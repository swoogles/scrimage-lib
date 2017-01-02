package imagemanipulation

import java.io.File
import java.net.URL

import com.sksamuel.scrimage.nio.JpegWriter
import com.sksamuel.scrimage.Color
import com.sksamuel.scrimage.Image

import ammonite.ops.Path

import Rows._
import Columns._


class ExampleScenes(basePath: Path, imgWidth: Int, imgHeight: Int) {
  val transformations = new Transformations(basePath)

  val boxes = new ScaledBoxes(imgWidth, imgHeight)

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
      val rect = boxes.smallRectangleAt(COL_1, ROW_6)
      val pprintedContent  = pprint.stringify(currentLocations, width=40) // TODO Handle with clean function. Don't pprint here.
      val textualDataStructure = CustomDrawable(rect, StandaloneDrawing.pprintDrawingWithoutBox, pprintedContent)
      textualDataStructure :: remainingNumbers
    }

  }

  def devicesForUsers() = transformations.multiStageImagesClass("user_devices_non_subclassed") { img =>
    val bill = "Bill Frasure"
    val garrett =  "Garrett Mctear"
    val andrew = "Andrew Proctor"

    val users = List(
      bill,
      garrett, 
      andrew
    )

    val user_devices = Map(
      bill -> List("970-104-1623", "970-222-3333"),
      garrett -> List("801-971-9844", "801-200-3273"),
      andrew -> List("336-687-3176", "336-654-5141")
    )

    val typedUsers = 
      CustomDrawable.spaceRowClassRectUpdated (
        users.map { name =>
          val drawable = CustomDrawable(boxes.wideRectangleAt(COL_1, ROW_1), StandaloneDrawing.pprintDrawingWithoutBox, name)
          pprint.pprintln(drawable)
          drawable
        }
      )

    val organizedNumbers = List[String]()

    val devicesWithRemainingUsers = typedUsers.scanLeft((organizedNumbers, typedUsers)){ case ((sortedNumbers, remainingUsers), user) =>
      val userDevices = user_devices.get(user.content).toList.flatten
      (sortedNumbers ::: userDevices, remainingUsers.tail)
    }

    val devicesDataStore = 
      CustomDrawable(boxes.wideRectangleAt(COL_5, ROW_7), StandaloneDrawing.pprintDrawingWithoutBox, user_devices)
      
    devicesWithRemainingUsers.map { case(currentLocations, remainingUsers) =>
      val textualDataStructure = 
        CustomDrawable(boxes.wideRectangleAt(COL_1, ROW_3), StandaloneDrawing.pprintDrawingWithoutBox, currentLocations)
      devicesDataStore :: textualDataStructure :: remainingUsers
    }
  }

  def tomatos() = transformations.multiStageImagesClass("rect_updated") { img =>
    import ammonite.ops.cwd
    val wd: ammonite.ops.Path = cwd / "TransformationImages"

    // implicit val wd = tranformationImagesFolder

    val rect = boxes.smallRectangleAt(COL_1, ROW_1)
    def demoImage(imgName: String) = {
    // val source = Source.fromURL(getClass.getResource("/data.xml"))
    import scala.io.Source
      // (wd / imgName).toIO

      val file: URL = getClass.getResource("/TransformationImages/" + imgName)
      new File(file.toURI)
    }

    // The string argument given to getResource is a path relative to
    // the resources directory.

    val seedsUnspaced = 
        List.fill(8) {
          CustomDrawable(rect, StandaloneDrawing.imgDrawerSepRect(demoImage("single_seed.png")))
        }

    val seeds = 
      CustomDrawable.spaceRowClassRectUpdated(
        seedsUnspaced
      )

    val dirtPiles = seeds
      .flatMap{ _.nextStageOpt(demoImage("dirt_pile.jpg")) }

    val seedlings = dirtPiles
      .flatMap{ _.nextStageOpt(demoImage("seedling.jpg")) }

    val saplings = seedlings
      .flatMap{ _.nextStageOpt(demoImage("sapling.jpg")) }

    val plants = saplings
      .flatMap{ _.nextStageOpt(demoImage("grown_plant.jpg")) }

    val tomatoes: List[CustomDrawable] = plants
      .flatMap{ _.nextStageList(demoImage("tomato.jpg")) }


    val tomatoesSpaced: List[CustomDrawable] = CustomDrawable.spaceRowClassRectUpdated(tomatoes)

    val stageImages = List(
      seeds,
      dirtPiles,
      seedlings,
      saplings,
      plants,
      tomatoesSpaced
    )

    stageImages.tail.scanLeft(stageImages.head){ case (acc, nextImages) =>
      acc ::: nextImages
    }

  }


}
