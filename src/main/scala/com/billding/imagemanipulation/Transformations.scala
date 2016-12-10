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
  val rectangleConfig = { g2: java.awt.Graphics2D =>
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

  def multiImageGeneratingFunction( imgName: String)( imgProducer: Image => List[Image]) = {
    val finalImages = imgProducer(blankImg)

    finalImages.zipWithIndex.foreach { case(finalImage, idx) =>
      val numberedImgName = s"${imgName}_$idx"
      imageGeneratingFunction(numberedImgName){ (blankImg)=>finalImage }
    }
    createGif(imgName)
  }


  def foldSummationImage() = multiImageGeneratingFunction("rectangles") { img =>

    val typedItems = Range(1, 11).map { curIdx =>
      NumericalListItem(
        smallRectangleAt(x=200+75*curIdx, y=50) ,
        // curIdx // Ascending values
        1 // Constant value
      )
    }.toList


     val accumulator = NumericalListItem(smallRectangleAt(x=50, y=50), 0)

     val foldingSummation = typedItems.scanLeft((accumulator, typedItems)){
       case ((acc, remainingItems), nextItem) => 
         (
           acc
             .copy(
             rect=acc.rect.copy(
               y=acc.rect.y+75
             ),
             value=acc.value+nextItem.value
           ), 
           remainingItems.tail.map(li=>
               li.copy(
                 rect=li.rect.copy(
                   y=li.rect.y+75
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

  def mapOverImages(imgFile: java.io.File) = imageGeneratingFunction("map_with_images") { img =>

    val typedItems = Range(1, 11).map { curIdx =>
      ImgDrawable(
        smallRectangleAt(x=200+75*curIdx, y=50) ,
        // curIdx // Ascending values
        imgFile
      )
    }.toList


     // val accumulator = NumericalListItem(smallRectangleAt(x=50, y=50), 0)

     // val foldingSummation = typedItems.scanLeft((accumulator, typedItems)){
     //   case ((acc, remainingItems), nextItem) => 
     //     (
     //       acc
     //         .copy(
     //         rect=acc.rect.copy(
     //           y=acc.rect.y+75
     //         ),
     //         value=acc.value+nextItem.value
     //       ), 
     //       remainingItems.tail.map(li=>
     //           li.copy(
     //             rect=li.rect.copy(
     //               y=li.rect.y+75
     //             )
     //           )
     //         )
     //       )
     // }

     typedItems.foldLeft(img) {
        case (curImg: Image, li: ImgDrawable) => li.draw(curImg)
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
        wideRectangleAt(x=200+200*idx, y=50)
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
      remainingNumbers.map(_.text)  ::: remainingNumbers.map(_.rect) ::: makeTextDrawable(locationMap, IMG_WIDTH/7, IMG_HEIGHT/2)
    }

    foldedLocationDrawablesWithRemaining.map { currentDrawables =>
      currentDrawables.foldLeft(img){
        case (curImg: Image, li: Drawable) => curImg.draw(li)
      }
    }

  }

  def devicesForUsers() = multiImageGeneratingFunction("user_devices") { img =>

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

    val typedUsers = users.zipWithIndex.map { case (name, idx) =>
      PhoneNumberListItem(
        name,
        wideRectangleAt(x=200+200*idx, y=50)
      )
    }

    val organizedNumbers = List[String]()

    val devicesWithRemainingUsers = typedUsers.scanLeft((organizedNumbers, typedUsers)){ case ((sortedNumbers, remainingUsers), user) =>
      val userDevices = user_devices.get(user.phoneNumber).toList.flatten
      (sortedNumbers ::: userDevices, remainingUsers.tail)
    }

    val foldedLocationDrawablesWithRemaining: List[List[Drawable]] = devicesWithRemainingUsers.zipWithIndex.map { case((currentLocations, remainingUsers), idx) =>
      val locationMap = pprint.stringify(currentLocations, width=40).split("\n").toList
      remainingUsers.map(_.text)  ::: remainingUsers.map(_.rect) ::: makeTextDrawable(locationMap, IMG_WIDTH/7, IMG_HEIGHT/4)
    }

    val user_devices_pretty_representation: List[String] = pprint.stringify(user_devices, width=30).split("\n").toList
    val user_devices_drawables: List[Drawable] = makeTextDrawable(user_devices_pretty_representation, IMG_WIDTH/7, IMG_HEIGHT * 5 / 8)
      

    // This makeks the DB rep display on one slide by itself. What I really need to do is add this to all the other lists.
    // val drawablesWithDBRepresentation: List[List[Drawable]] =  :: foldedLocationDrawablesWithRemaining

    foldedLocationDrawablesWithRemaining.map { currentDrawables =>
      (currentDrawables ::: user_devices_drawables)
      .foldLeft(img){
        case (curImg: Image, li: Drawable) => curImg.draw(li)
      }
    }

  }
}

