package imagemanipulation

import com.sksamuel.scrimage.nio.JpegWriter
import com.sksamuel.scrimage.Color
import com.sksamuel.scrimage.Image

sealed trait Product { 
  val woodRequired: Int
}
case object Cabinet extends Product {
  val woodRequired = 2
}
case object ConstructionMatierals extends Product {
  val woodRequired = 3
}
case object Fencing extends Product {
  val woodRequired = 2
}
case object Flooring extends Product {
  val woodRequired = 1
}
case object Furniture extends Product {
  val woodRequired = 4
}
case object Ship extends Product {
  val woodRequired = 10
}
case object FancyFurniture extends Product {
  val woodRequired = 8
}
case object Trim extends Product {
  val woodRequired = 1
}

sealed trait WoodType {
  val possibleProducts : List[Product]
  }
case object Pine extends WoodType { // Furniture and Construction 
  val possibleProducts = List(Furniture, ConstructionMatierals)
}
case object Poplar extends WoodType { // Hardest softwood. Used for cabinets, and can be stained to look like hardwood.
  val possibleProducts = List(Cabinet, Trim)
}

case object Cedar extends WoodType { // Fences, Ships, Shingles
  val possibleProducts = List(Fencing, Ship)
}
case object Oak extends WoodType { // Fine Furniture, Desks, Flooring
  val possibleProducts = List(Furniture, Flooring)
}
case object Birch extends WoodType { // Cabinets and High-End Furniture
  val possibleProducts = List(Cabinet, FancyFurniture)
}

sealed trait WoodenItem {
  val woodType: WoodType
}
case class Tree(woodType: WoodType, unitsOfWood: Int) extends WoodenItem {
}

case class Log(woodType: WoodType) extends WoodenItem

case class WoodenProduct(product: Product, woodType: WoodType) extends WoodenItem with Product {
  val woodRequired = product.woodRequired
  assert(woodType.possibleProducts.contains(product))
}

object WoodFunctions {
  def cutIntoLogs(tree: Tree): List[Log] = {
    List.fill(tree.unitsOfWood)(Log(tree.woodType))
  }
}

object ImageBasedExamples extends TextDrawing with FileSystemOperations with BoundaryBoxes {
  import scala.util.{Try, Success, Failure}
  val scenario1 = {
    val forest: List[Tree] = (
      List.fill(5)(Tree(Pine, 50))
      ::: List.fill(4)(Tree(Poplar, 60))
      ::: List.fill(3)(Tree(Cedar, 70))
      ::: List.fill(2)(Tree(Oak, 80))
      ::: List.fill(1)(Tree(Birch, 90))
  )

    val desiredProducts = List(
      Cabinet,
      Cabinet,
      Fencing,
      Flooring
    )

    val logs: List[Log] = forest.flatMap { tree =>
      WoodFunctions.cutIntoLogs(tree)
    }

    val finishedProducts: List[WoodenProduct] = List()
    val startingPoint: Try[(List[WoodenProduct], List[Log])] = Success((finishedProducts, logs))
    desiredProducts.foldLeft(startingPoint){ case (Success((finishedProducts, remainingLogs)), desiredProduct) =>
      val (usableLogs, unusableLogs) = remainingLogs.partition(log=>log.woodType.possibleProducts.contains(desiredProduct))
      if ( usableLogs.length > desiredProduct.woodRequired ) {
        // TODO Figure out if combo wood products are worth the trouble
        // val newlyFinishedproduct = WoodenProduct(desiredProduct, usab
        // TODO add new product to finished products
        Success((finishedProducts, usableLogs.drop(desiredProduct.woodRequired) ::: unusableLogs))
      }
      else
        Failure(new Exception("Not enough wood to make: " + desiredProduct))
    }
  }
  val tree = List(
    "Forest", // Forest
    "Trees", // List[Tree]
    "Logs", // List[Log]
    "Wooden Products", // List[List[WoodComponent]] eg. ChairLeg, BaseballBat, Guitars, Doors
    "Ash Pile" // AshPile (Result of a fire burning down the factory containing all the products.
    )

  val treesVaried = List(
    "Forest", // Forest
    "Trees", // List[Tree] (Trees can be Oak, Spruce, Fur)
    "Logs", // List[Log] (Logs can be Oak, Spruce, Fur)
    "Wooden Products", // List[List[WoodComponent]] eg. ChairLeg, BaseballBat, Guitars, Doors
    "Ash Pile" // AshPile (Result of a fire burning down the factory containing all the products.
    )

  val mining = List(
    "Mountain", // Mountain
    "Dug Mountain", // Mined[Mountain]
    "Ore fragments", // Set[Ore] (Result of blasting)
    "Ore loads", // Stream[LoadedMineCart]
    "Full Smelter" // FullSmelter
  )
}
