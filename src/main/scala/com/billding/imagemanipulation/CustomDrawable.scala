package imagemanipulation

import java.awt.{Color => JColor, Font => JFont, Image => JImage}
import com.sksamuel.scrimage.canvas.drawable.Rect
import com.sksamuel.scrimage.canvas.drawable.Text
import com.sksamuel.scrimage.canvas.Canvas

sealed trait CustomDrawable {
  val rect: Rect
  def draw(canvas: Canvas): Canvas
}

object CustomDrawableRectUpdated {
  def apply(rect: Rect, drawWithRect: Rect => String => Canvas => Canvas, content: Iterable[_]): CustomDrawableRectUpdated = {
    import pprint.Config
    implicit val pprintConfig = Config()
    CustomDrawableRectUpdated(rect, drawWithRect, pprint.stringify(content, width=40))
  }
}

/*
Passing in a String for content will leave it unchanged, but passing an Iterable[_] will
result in the content being pprint formatted before drawing.
*/
case class CustomDrawableRectUpdated(rect: Rect, drawWithRect: Rect => String => Canvas => Canvas, content: String = "DEFAULT") {
  def draw: Canvas => Canvas = {
    drawWithRect(rect)(content)
  }

  def onNextRow = {
    val nextRect = rect.copy(y=rect.y+75)
    copy(rect = nextRect)
  }

  def nextStageOpt(img: java.io.File) = {
    if( scala.math.abs(scala.util.Random.nextInt % 100) > 20 ) {
      val nextRowVersion = onNextRow
      Some(nextRowVersion.copy(drawWithRect=StandaloneDrawing.imgDrawerSepRect(img)))
    }
    else 
      None
  }

  def nextStageList(img: java.io.File) = {
    val nextRowVersion = onNextRow
    List.fill(3)(nextRowVersion.copy(drawWithRect=StandaloneDrawing.imgDrawerSepRect(img)))
  }
}

object StandaloneDrawing extends TextDrawing {

  def imgDrawerSepRect(imgFile: java.io.File): Rect=>String=>Canvas=>Canvas = {rect => 
  { content => canvas =>
      import com.sksamuel.scrimage.Image
      import com.sksamuel.scrimage.ScaleMethod.FastScale
      val image1 = Image.fromFile(imgFile)
        .scaleTo(rect.width,rect.height, FastScale)

        canvas.draw(rect).overlay(image1, rect.x, rect.y)
    }
  }

  val pprintDrawing: Rect=>String=>Canvas=>Canvas = 
  { rect => content => canvas =>
    import pprint.Config
    implicit val pprintConfig = Config()

    val imgFont = new JFont("Sans-seriff", 1, 14)

    def textLines: List[String] = pprint.stringify(content, width=40).split("\n").toList
    val drawableTextLines: List[Text] = makeTextDrawable(textLines, rect.x+15, rect.y+30)
    drawableTextLines.map { text=>text.x }

    drawableTextLines.foldLeft(canvas.draw(rect)){ (curCanvas, nextText) => 
      curCanvas.draw(nextText) 
    }
  }

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

object CustomDrawable {
  import monocle.Lens
  import monocle.macros.GenLens
  def spaceRow[T <: CustomDrawable]( imgItems: List[T] ): List[T] = {
    val (head :: tail) = imgItems
    val (finalRect, spacedList: List[T]) = tail.fold((head, List(head): List[T])) { case ((lastDrawable: T, accItems: List[T]), nextItem: T) =>
      val newRect = nextItem.rect.copy(x = lastDrawable.rect.x + lastDrawable.rect.width + 10)
      val newItem = nextItem match {
        case textDrawable: TextDrawable => textDrawable match {
          case nli: NumericalListItem => nli.copy(rect=newRect)
        }
      }
      (newItem, accItems :+ newItem)

    }
    spacedList

  }

  def spaceRowClassRectUpdated( imgItems: List[CustomDrawableRectUpdated] ): List[CustomDrawableRectUpdated] = {
    val rectLens = GenLens[CustomDrawableRectUpdated](x=>x.rect)
    val xLens = GenLens[Rect](rect=>rect.x)
    val (head :: tail) = imgItems
    tail.foldLeft(List(head)) { (accItems, nextItem) =>
      val lastRect = accItems.last.rect
      val newXValue = lastRect.x + lastRect.width + 10
      val spacedItem = (rectLens composeLens xLens).modify(oldX=>newXValue)(nextItem)
      accItems :+ spacedItem
    }
  }

}

case class NumericalListItem(rect: Rect, value: Int = 1) extends TextDrawable {
  val content = value.toString

  def onNextRow = {
    val nextRect = rect.copy(y=rect.y+75)
    copy(rect = nextRect)
  }

}
