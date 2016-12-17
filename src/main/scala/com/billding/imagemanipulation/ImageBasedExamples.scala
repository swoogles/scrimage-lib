package imagemanipulation

import com.sksamuel.scrimage.nio.JpegWriter
import com.sksamuel.scrimage.Color
import com.sksamuel.scrimage.Image

sealed trait Product { 
  val woodRequired: Int
}
case object Cabinet extends Product {
  val woodRequired = 50
}
case object ConstructionMatierals extends Product {
  val woodRequired = 100
}
case object Fencing extends Product {
  val woodRequired = 100
}
case object Flooring extends Product {
  val woodRequired = 10
}
case object Furniture extends Product {
  val woodRequired = 50
}
case object Ship extends Product {
  val woodRequired = 1000
}
case object Shingles extends Product {
  val woodRequired = 1
}
case object FancyFurniture extends Product {
  val woodRequired = 200
}
case object Trim extends Product {
  val woodRequired = 10
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
  val possibleProducts = List(Fencing, Ship, Shingles)
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

// case class Chair(woodType: WoodType) extends WoodenProduct

object ImageBasedExamples extends TextDrawing with FileSystemOperations with BoundaryBoxes {
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
