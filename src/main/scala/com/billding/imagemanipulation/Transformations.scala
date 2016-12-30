package imagemanipulation

import java.io.File
import java.net.URL

import com.sksamuel.scrimage.nio.JpegWriter
import com.sksamuel.scrimage.Color
import com.sksamuel.scrimage.Image

import ammonite.ops.Path

import Rows._
import Columns._

case class TransformationResults(steps: List[Path], gif: Path)

class Transformations(basePath: Path) extends TextDrawing with FileSystemOperations with BoundaryBoxes {
  import ammonite.ops.%
  import ammonite.ops.mkdir


  private val generatedImagesFolder = basePath / "GeneratedImages"
  private val tranformationImagesFolder = basePath / "TransformationImages"
  mkdir! generatedImagesFolder

  val IMG_HEIGHT = 500
  val IMG_WIDTH = 800

  val boxes = new ScaledBoxes(IMG_WIDTH, IMG_HEIGHT)

  val blankImg = Image(IMG_WIDTH, IMG_HEIGHT)
      .fit(IMG_WIDTH, IMG_HEIGHT, Color.Black)

  val IMG_EXTENSION = ".jpg"

  private def imageGeneratingFunction( imgName: String)( imgProducer: Image => Image) = {
    val finalImage = imgProducer(blankImg)
    val imgPath = generatedImagesFolder / (s"$imgName$IMG_EXTENSION")
    finalImage.output(imgPath.toIO)(JpegWriter())
    imgPath
  }

  private def createGif(imgName: String) = {
    implicit val wd: Path = basePath / "GeneratedImages"
    val outFile  = basePath / "GeneratedImages" / s"$imgName.gif"
    %('convert, "-delay", "120", "-loop", "0", s"${imgName}*$IMG_EXTENSION", s"$imgName.gif")
    outFile
  }

  def multiStageImagesClass( imgName: String)( imgProducer: Image => List[List[CustomDrawable]]) = {
    val shapeLists: List[List[CustomDrawable]] = imgProducer(blankImg)

    val stagedImages: List[Image] = drawMultipleImagesClass(blankImg, shapeLists)

    val createdImages: List[Path] = stagedImages.zipWithIndex.map { case(finalImage, idx) =>
      val numberedImgName = s"${imgName}_$idx"
      imageGeneratingFunction(numberedImgName){ (blankImg)=>
        finalImage 
      }
    }
    TransformationResults(createdImages, createGif(imgName))
  }

  private def drawMultipleImagesClass(img: Image, stagedDrawables: List[List[CustomDrawable]]): List[Image] = {
    stagedDrawables.map { currentDrawables =>
      currentDrawables.foldLeft(img){
        case (curImg: Image, li: CustomDrawable) => li.draw(curImg)
      }
    }
  }

  def devicesForUsers() = multiStageImagesClass("user_devices_non_subclassed") { img =>

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

    val typedUsers = 
      CustomDrawable.spaceRowClassRectUpdated (
        users.map { name =>
          val drawable = CustomDrawable(boxes.wideRectangleAt(COL_1, ROW_1), StandaloneDrawing.pprintDrawing, name)
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
      CustomDrawable(boxes.wideRectangleAt(COL_5, ROW_7), StandaloneDrawing.pprintDrawing, user_devices)
      
    devicesWithRemainingUsers.map { case(currentLocations, remainingUsers) =>
      val textualDataStructure = 
        CustomDrawable(boxes.wideRectangleAt(COL_1, ROW_3), StandaloneDrawing.pprintDrawing, currentLocations)
      devicesDataStore :: textualDataStructure :: remainingUsers
    }
  }

  def tomatos() = multiStageImagesClass("rect_updated") { img =>
    implicit val wd = tranformationImagesFolder

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

    val img1 = demoImage("single_seed.png")
    val img2 = demoImage("dirt_pile.jpg")
    val img3 = demoImage("seedling.jpg")
    val img4 = demoImage("sapling.jpg")
    val img5 = demoImage("grown_plant.jpg")
    val img6 = demoImage("tomato.jpg")
    

    val seedsUnspaced = 
        List.fill(8) {
          CustomDrawable(rect, StandaloneDrawing.imgDrawerSepRect(img1))
        }

    val seeds = 
      CustomDrawable.spaceRowClassRectUpdated(
        seedsUnspaced
      )

    val dirtPiles = seeds
      .flatMap{ _.nextStageOpt(img2) }

    val seedlings = dirtPiles
      .flatMap{ _.nextStageOpt(img3) }

    val saplings = seedlings
      .flatMap{ _.nextStageOpt(img4) }

    val plants = saplings
      .flatMap{ _.nextStageOpt(img5) }

    val tomatoes: List[CustomDrawable] = plants
      .flatMap{ _.nextStageList(img6) }


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

