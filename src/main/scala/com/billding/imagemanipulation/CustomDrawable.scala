package imagemanipulation

import java.awt.{Color => JColor, Font => JFont, Image => JImage}
import com.sksamuel.scrimage.canvas.drawable.Rect
import com.sksamuel.scrimage.canvas.drawable.Text
import com.sksamuel.scrimage.canvas.Canvas

sealed trait CustomDrawable {
  val rect: Rect
  def draw(canvas: Canvas): Canvas
}

sealed trait TextDrawable extends CustomDrawable {
  val rect: Rect
  val content: String
  val imgFont = new JFont("Sans-seriff", 1, 28)

  def text: Text =
       Text(content.toString, rect.x+15, rect.y+30, { g2 =>
         g2.setBackground(JColor.BLUE)
         g2.setFont(imgFont)
       })

  override def draw(canvas: Canvas): Canvas = {
    canvas.draw(rect).draw(text)
  }

}

sealed trait PprintTextDrawable extends CustomDrawable with BoundaryBoxes {
  import pprint.Config
  val x: Int
  val y: Int
  override implicit val pprintConfig = Config()

  val rect: Rect =
    Rect(x=x, y=y, width=50, height=50, rectangleConfig )
  val content: Iterable[_]
  override val imgFont = new JFont("Sans-seriff", 1, 28)

  def textLines: List[String] = pprint.stringify(content, width=40).split("\n").toList
  val drawableTextLines: List[Text] = makeTextDrawable(textLines, rect.x+15, rect.y+30)
  drawableTextLines.map { text=>text.x }

  override def draw(canvas: Canvas): Canvas = {
    drawableTextLines.foldLeft(canvas.draw(rect)){ (curCanvas, nextText) => 
      curCanvas.draw(nextText) 
    }
  }

}

case class TextualDataStructure(x: Int, y: Int, content: Iterable[_]) extends PprintTextDrawable

case class ImgDrawable(rect: Rect, imgFile: java.io.File) extends CustomDrawable {
  import com.sksamuel.scrimage.Image
  import com.sksamuel.scrimage.ScaleMethod.FastScale
  val image1 = Image.fromFile(imgFile)
    .scaleTo(rect.width,rect.height, FastScale)

  override def draw(canvas: Canvas) = {
    canvas.draw(rect).overlay(image1, rect.x, rect.y)
  }

  def onNextRow = {
    val nextRect = rect.copy(y=rect.y+75)
    copy(rect = nextRect)
  }

  def nextStageOpt(img: java.io.File) = {
    if( scala.math.abs(scala.util.Random.nextInt % 100) > 20 )
      Some(onNextRow.copy(imgFile = img))
    else 
      None
  }

  def nextStageList(img: java.io.File) = {
    List.fill(3)(onNextRow.copy(imgFile = img))
  }

}

object CustomDrawable {
  /*
   TODO: Figure out how to accomplish this without all the casting
   */
  def spaceRow[T <: CustomDrawable]( imgItems: List[T] ): List[T] = {
    val (head :: tail) = imgItems
    val (finalRect, spacedList: List[T]) = tail.fold((head, List(head): List[T])) { case ((lastDrawable: T, accItems: List[T]), nextItem: T) =>
      val newRect = nextItem.rect.copy(x = lastDrawable.rect.x + lastDrawable.rect.width + 10)
      // val newItem = nextItem.copy(rect=newRect)
      val newItem: T = head match {
        case pprintable: PprintTextDrawable => pprintable match {
          case textual: TextualDataStructure => textual.copy(x=newRect.x, y=newRect.y).asInstanceOf[T]
        }
        case textDrawable: TextDrawable => textDrawable match {
          case nli: NumericalListItem => nli.copy(rect=newRect).asInstanceOf[T]
          case pli: PhoneNumberListItem => pli.copy(rect=newRect).asInstanceOf[T]
        }
        case imgDrawable: ImgDrawable => imgDrawable.copy(rect=newRect).asInstanceOf[T]
      }
      (newItem, accItems :+ newItem)

    }
    spacedList

  }

}

case class NumericalListItem(rect: Rect, value: Int = 1) extends TextDrawable {
  val content = value.toString

  def onNextRow = {
    val nextRect = rect.copy(y=rect.y+75)
    copy(rect = nextRect)
  }

}

case class PhoneNumberListItem(phoneNumber: String, rect: Rect, value: Int = 1) extends TextDrawable {
  override val imgFont = new JFont("Sans-seriff", 1, 14)
  val content = phoneNumber
}

