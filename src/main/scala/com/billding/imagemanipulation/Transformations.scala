package imagemanipulation

import com.sksamuel.scrimage.nio.JpegWriter
import com.sksamuel.scrimage.Color
import com.sksamuel.scrimage.Image

object Transformations extends TextDrawing with FileSystemOperations with BoundaryBoxes {
  import ammonite.ops.%
  import ammonite.ops.cwd

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

  def multiStageImagesClass( imgName: String)( imgProducer: Image => List[List[CustomDrawable]]) = {
    val shapeLists: List[List[CustomDrawable]] = imgProducer(blankImg)

    val stagedImages: List[Image] = drawMultipleImagesClass(blankImg, shapeLists)

    stagedImages.zipWithIndex.foreach { case(finalImage, idx) =>
      val numberedImgName = s"${imgName}_$idx"
      imageGeneratingFunction(numberedImgName){ (blankImg)=>
        finalImage 
      }
    }
    createGif(imgName)
  }

  private def drawMultipleImagesClass(img: Image, stagedDrawables: List[List[CustomDrawable]]): List[Image] = {
    stagedDrawables.map { currentDrawables =>
      currentDrawables.foldLeft(img){
        case (curImg: Image, li: CustomDrawable) => li.draw(curImg)
      }
    }
  }


  def foldSummationImage() = multiStageImagesClass("rectangles") { img =>

    val typedItems = 
    CustomDrawable.spaceRowClassRectUpdated(
      List.fill(9) {
        CustomDrawable(
          1,
          smallRectangleAt(x=200, y=50)
        )
      }
    )

     val accumulator = CustomDrawable(0, smallRectangleAt(x=50, y=50))

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

  def phoneNumbersMultiStageNonSubclass() = multiStageImagesClass("phone_folding_non_subclassed") { img =>
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
      CustomDrawable(wideRectangleAt(x=200, y=50), StandaloneDrawing.pprintDrawing, number)
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
      val rect: Rect = Rect(x=IMG_WIDTH/7, y=IMG_HEIGHT/2, width=50, height=50, rectangleConfig )
      val pprintedContent  = pprint.stringify(currentLocations, width=40) // TODO Handle with clean function. Don't pprint here.
      val textualDataStructure = CustomDrawable(rect, StandaloneDrawing.pprintDrawing, pprintedContent)
      textualDataStructure :: remainingNumbers
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
          CustomDrawable(wideRectangleAt(x=200, y=50), StandaloneDrawing.pprintDrawing, name)
        }
      )

    val organizedNumbers = List[String]()

    val devicesWithRemainingUsers = typedUsers.scanLeft((organizedNumbers, typedUsers)){ case ((sortedNumbers, remainingUsers), user) =>
      val userDevices = user_devices.get(user.content).toList.flatten
      (sortedNumbers ::: userDevices, remainingUsers.tail)
    }

    val devicesDataStore = 
      CustomDrawable(wideRectangleAt(x=IMG_WIDTH/7, y=IMG_HEIGHT * 5 / 8), StandaloneDrawing.pprintDrawing, user_devices)
      
    devicesWithRemainingUsers.map { case(currentLocations, remainingUsers) =>
      val textualDataStructure = 
        CustomDrawable(wideRectangleAt(x=IMG_WIDTH/7, y=IMG_HEIGHT / 4), StandaloneDrawing.pprintDrawing, currentLocations)
      devicesDataStore :: textualDataStructure :: remainingUsers
    }
  }

  def againWithoutSubclassingRectUpdated() = multiStageImagesClass("rect_updated") { img =>
  import ammonite.ops.cwd
    implicit val wd: ammonite.ops.Path = cwd / "TransformationImages"
    val rect = smallRectangleAt(50, 50)
    def demoImage(imgName: String) = (wd / imgName).toIO
    val img1 = demoImage("single_seed.png")
    val img2 = demoImage("dirt_pile.jpg")
    val img3 = demoImage("seedling.jpg")
    val img4 = demoImage("sapling.jpg")
    val img5 = demoImage("grown_plant.jpg")
    val img6 = demoImage("tomato.jpg")
    

    val seedsUnspaced = 
        List.fill(11) {
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

