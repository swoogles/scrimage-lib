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

class Transformations(basePath: Path) extends TextDrawing with FileSystemOperations {
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
    val outFile  = generatedImagesFolder / s"$imgName.gif"
    %('convert, "-delay", "120", "-loop", "0", s"${imgName}*$IMG_EXTENSION", s"$imgName.gif")(generatedImagesFolder)
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

}

