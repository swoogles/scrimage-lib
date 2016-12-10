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
        import ammonite.ops._
        // import ammonite.ops.ImplicitWd._
        implicit val wd: ammonite.ops.Path = cwd / "GeneratedImages"
        val res = %%('convert, "-delay", "120", "-loop", "0", s"${imgName}*.jpg", s"$imgName.gif")
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
        Rect(x=200+200*idx, y=50, width=150, height=50, { g2 =>
          g2.setColor(JColor.GREEN)
          g2.setFont(imgFont)
        })
      )
    }

    val organizedNumbers = List[String]()

    val devicesWithRemainingUsers = typedUsers.scanLeft((organizedNumbers, typedUsers)){ case ((sortedNumbers, remainingUsers), user) =>
      val userDevices = user_devices.get(user.phoneNumber).toList.flatten
      (sortedNumbers ::: userDevices, remainingUsers.tail)
    }

    val foldedLocationDrawablesWithRemaining: List[List[Drawable]] = devicesWithRemainingUsers.zipWithIndex.map { case((currentLocations, remainingUsers), idx) =>
      val locationMap = pprint.stringify(currentLocations, width=40).split("\n").toList
      remainingUsers.map(_.text)  ::: remainingUsers.map(_.rect) ::: makeTextDrawable(locationMap, 200, 200)
    }

    val user_devices_pretty_representation: List[String] = pprint.stringify(user_devices, width=30).split("\n").toList
    val user_devices_drawables: List[Drawable] = makeTextDrawable(user_devices_pretty_representation, 200, 500)
      

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

