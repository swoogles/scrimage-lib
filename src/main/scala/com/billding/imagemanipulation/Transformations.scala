package imagemanipulation

import com.sksamuel.scrimage.nio.JpegWriter
import com.sksamuel.scrimage.Color
import com.sksamuel.scrimage.Image
import java.awt.{Color => JColor}

import com.sksamuel.scrimage.canvas.Canvas._
import com.sksamuel.scrimage.canvas.Font

import com.sksamuel.scrimage.canvas.drawable.Rect
import com.sksamuel.scrimage.canvas.Drawable
import com.sksamuel.scrimage.canvas.drawable.Text

trait BoundaryBoxes extends TextDrawing {
  protected val rectangleConfig = { g2: java.awt.Graphics2D =>
      g2.setColor(JColor.GREEN)
      g2.setFont(imgFont)
    }

  protected def smallRectangleAt(x: Int, y: Int) = Rect(x=x, y=y, width=50, height=50, rectangleConfig )

  protected def wideRectangleAt(x: Int, y: Int) = Rect(x=x, y=y, width=150, height=50, rectangleConfig )

}

object Transformations extends TextDrawing with FileSystemOperations with BoundaryBoxes {
  import ammonite.ops._

  val IMG_HEIGHT = 800
  val IMG_WIDTH = 1400

  val blankImg = Image(IMG_WIDTH, IMG_HEIGHT)
      .fit(IMG_WIDTH, IMG_HEIGHT, Color.Black)

  val IMG_EXTENSION = ".jpg"

  def imageGeneratingFunction( imgName: String)( imgProducer: Image => Image) = {
    val finalImage = imgProducer(blankImg)
    val imgPath = generatedImgDir / (s"$imgName$IMG_EXTENSION")
    finalImage.output(imgPath.toIO)(JpegWriter())
  }

  private def createGif(imgName: String) = {
    implicit val wd: ammonite.ops.Path = cwd / "GeneratedImages"
    %('convert, "-delay", "120", "-loop", "0", s"${imgName}*$IMG_EXTENSION", s"$imgName.gif")
  }

  def multiStageImages( imgName: String)( imgProducer: Image => List[List[CustomDrawable]]) = {
    val shapeLists: List[List[CustomDrawable]] = imgProducer(blankImg)

    val stagedImages: List[Image] = drawMultipleImages(blankImg, shapeLists)

    stagedImages.zipWithIndex.foreach { case(finalImage, idx) =>
      val numberedImgName = s"${imgName}_$idx"
      imageGeneratingFunction(numberedImgName){ (blankImg)=>
        finalImage 
      }
    }
    createGif(imgName)
  }

  private def drawMultipleImages(img: Image, stagedDrawables: List[List[CustomDrawable]]): List[Image] = {
    stagedDrawables.map { currentDrawables =>
      currentDrawables.foldLeft(img){
        case (curImg: Image, li: CustomDrawable) => li.draw(curImg)
      }
    }
  }

  def foldSummationImage() = multiStageImages("rectangles") { img =>

    val typedItemsUnspaced = 
      List.fill(11) {
        NumericalListItem(
          smallRectangleAt(x=200, y=50) ,
          1 // Constant value
        )
      }

    val typedItems = CustomDrawable.spaceRow(typedItemsUnspaced)

     val accumulator = NumericalListItem(smallRectangleAt(x=50, y=50), 0)

     val foldingSummation = typedItems.scanLeft((accumulator, typedItems)){
       case ((acc: NumericalListItem, remainingItems: List[NumericalListItem]), nextItem: NumericalListItem) => 
         (
           acc
             .onNextRow
             .copy( value=acc.value+nextItem.value), 
           remainingItems.tail.map(_.onNextRow)
         )
     }

    foldingSummation.map { tup => tup._1 +: tup._2 }
  }

  def tomatoGrowing() = multiStageImages("tomato_growing") { img =>
    implicit val wd: ammonite.ops.Path = cwd / "TransformationImages"
    val img1 = (wd / "single_seed.png").toIO
    val img2 = (wd / "dirt_pile.jpg").toIO
    val img3 = (wd / "seedling.jpg").toIO
    val img4 = (wd / "sapling.jpg").toIO
    val img5 = (wd / "grown_plant.jpg").toIO
    val img6 = (wd / "tomato.jpg").toIO
    

    val seedsUnspaced = 
        List.fill(11) {
          ImgDrawable(
            smallRectangleAt(x=200, y=50),
            img1
          )
        }

    val seeds = CustomDrawable.spaceRow(seedsUnspaced)

    val dirtPiles = seeds
      .flatMap{ _.nextStageOpt(img2) }

    val seedlings = dirtPiles
      .flatMap{ _.nextStageOpt(img3) }

    val saplings = seedlings
      .flatMap{ _.nextStageOpt(img4) }

    val plants = saplings
      .flatMap{ _.nextStageOpt(img5) }

    val tomatoes: List[ImgDrawable] = plants
      .flatMap{ _.nextStageList(img6) }


    val tomatoesSpaced: List[ImgDrawable] = CustomDrawable.spaceRow(tomatoes)

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


  def phoneNumbersMultiStage() = multiStageImages("phone_folding") { img =>
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

    val typedPhoneNumbersUnspaced = phoneNumbers.map { number =>
      PhoneNumberListItem(
        number,
        wideRectangleAt(x=200, y=50)
      )
    }
    val typedPhoneNumbers = CustomDrawable.spaceRow(typedPhoneNumbersUnspaced)

    val organizedNumbers = Map[String, List[String]]().withDefaultValue(Nil)

    val locationFoldingWithRemaining: List[(Map[String,List[String]], List[PhoneNumberListItem])] =
      typedPhoneNumbers.scanLeft((organizedNumbers, typedPhoneNumbers)){ case ((sortedNumbers, remainingNumbers), li) =>
        val region = areaCodesAndStates.get(li.phoneNumber.take(3)).getOrElse("UNKNOWN")
        val neighboringEntries = sortedNumbers(region)
        (sortedNumbers + (region -> (li.phoneNumber :: neighboringEntries)), remainingNumbers.tail)
      }

    locationFoldingWithRemaining.map { case(currentLocations, remainingNumbers) =>
      val textualDataStructure = TextualDataStructure(IMG_WIDTH/7, IMG_HEIGHT/2, currentLocations)
      textualDataStructure :: remainingNumbers
    }

  }

  def devicesForUsers() = multiStageImages("user_devices") { img =>

    val users = List(
      "Bill Frasure",
      "Garrett Mctear",
      "Andrew Proctor"
    )

    val user_devices = Map(
      "Bill Frasure" -> List("970-104-1623", "970-222-3333"),
      "Garrett Mctear" -> List("801-971-9844", "801-200-3273"),
      "Andrew Proctor" -> List("336-687-3176", "336-654-5141")
      )

    val typedUsersUnspaced = users.map { name =>
      PhoneNumberListItem(
        name,
        wideRectangleAt(x=200, y=50)
      )
    }

    val typedUsers = CustomDrawable.spaceRow(typedUsersUnspaced)

    val organizedNumbers = List[String]()

    val devicesWithRemainingUsers = typedUsers.scanLeft((organizedNumbers, typedUsers)){ case ((sortedNumbers, remainingUsers), user) =>
      val userDevices = user_devices.get(user.phoneNumber).toList.flatten
      (sortedNumbers ::: userDevices, remainingUsers.tail)
    }

    val devicesDataStore = TextualDataStructure(IMG_WIDTH/7, IMG_HEIGHT * 5 / 8, user_devices)
    devicesWithRemainingUsers.map { case(currentLocations, remainingUsers) =>
      val textualDataStructure = TextualDataStructure(IMG_WIDTH/7, IMG_HEIGHT/4, currentLocations)
      devicesDataStore :: textualDataStructure :: remainingUsers
    }

  }
}

