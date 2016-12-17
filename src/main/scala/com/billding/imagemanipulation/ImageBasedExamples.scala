package imagemanipulation

import com.sksamuel.scrimage.nio.JpegWriter
import com.sksamuel.scrimage.Color
import com.sksamuel.scrimage.Image

sealed trait Products
case object Cabinet extends Product
case object ConstructionMatierals extends Product
case object Fencing extends Product
case object Flooring extends Product
case object Furniture extends Product
case object Ship extends Product
case object Shingles extends Product
case object Desk extends Product
case object FancyFurniture extends Product
case object Trim extends Product

sealed trait WoodType
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
  val possibleProducts = List(Desk, Flooring)
}
case object Birch extends WoodType { // Cabinets and High-End Furniture
  val possibleProducts = List(Cabinet, FancyFurniture)
}

sealed trait WoodenItem {
  val woodType: WoodType
}
case class Tree(woodType: WoodType) extends WoodenItem {
}

case class Log(woodType: WoodType) extends WoodenItem {
}

trait WoodenProduct extends WoodenItem {
}
case class Chair(woodType: WoodType) extends WoodenProduct

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
