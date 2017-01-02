package imagemanipulation

import java.awt.{Color => JColor, Font => JFont, Image => JImage}
import com.sksamuel.scrimage.canvas.drawable.Rect
import com.sksamuel.scrimage.canvas.drawable.Text
import com.sksamuel.scrimage.canvas.Canvas

/*
Passing in a String for content will leave it unchanged, but passing an Iterable[_] will
result in the content being pprint formatted before drawing.
*/
case class CustomDrawable(rect: Rect, drawWithRect: Rect => String => Canvas => Canvas, content: String = "DEFAULT", value: Int = 1) {
  def draw: Canvas => Canvas = {
    drawWithRect(rect)(content)
  }

  def onNextRow = {
    val nextRect = rect.copy(y=rect.y+75)
    copy(rect = nextRect)
  }

  def nextStageOpt(img: java.io.File) = {
    // TODO: Handle this math/impurity elsewhere! It doesn't belong in this Option transformation.
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

  def imgDrawerSepRect(imgFile: java.io.File): Rect=>String=>Canvas=>Canvas =
  { rect => content => canvas =>
      import com.sksamuel.scrimage.Image
      import com.sksamuel.scrimage.ScaleMethod.FastScale
      val image1 = Image.fromFile(imgFile)
        .scaleTo(rect.width,rect.height, FastScale)

        canvas.draw(rect).overlay(image1, rect.x, rect.y)
  }

  val pprintDrawingWithoutBox: Rect=>String=>Canvas=>Canvas = 
  { rect => content => canvas =>
    import pprint.Config
    implicit val pprintConfig = Config()

    def textLines: List[String] = pprint.stringify(content, width=40).split("\n").toList
    val drawableTextLines: List[Text] = makeTextDrawable(textLines, rect.x+15, rect.y+30)
    drawableTextLines.map { text=>text.x }

    drawableTextLines.foldLeft(canvas){ (curCanvas, nextText) => 
      curCanvas.draw(nextText) 
    }
  }

  val pprintDrawingWithBox: Rect=>String=>Canvas=>Canvas = 
  { rect => content => canvas =>
    import pprint.Config
    implicit val pprintConfig = Config()

    def textLines: List[String] = pprint.stringify(content, width=40).split("\n").toList
    val drawableTextLines: List[Text] = makeTextDrawable(textLines, rect.x+15, rect.y+30)
    drawableTextLines.map { text=>text.x }

    drawableTextLines.foldLeft(canvas.draw(rect)){ (curCanvas, nextText) => 
      curCanvas.draw(nextText) 
    }
  }

}

object CustomDrawable {
  import monocle.Lens
  import monocle.macros.GenLens

  val marginBetweenItems = 10
  def spaceRowClassRectUpdated( imgItems: List[CustomDrawable] ): List[CustomDrawable] = {
    val rectLens = GenLens[CustomDrawable](x=>x.rect)
    val xLens = GenLens[Rect](rect=>rect.x)
    val (head :: tail) = imgItems
    tail.foldLeft(List(head)) { (accItems, nextItem) =>
      val lastRect = accItems.last.rect
      val newXValue = lastRect.x + lastRect.width + marginBetweenItems
      val spacedItem = (rectLens composeLens xLens).modify(oldX=>newXValue)(nextItem)
      accItems :+ spacedItem
    }
  }

  def apply(rect: Rect, drawWithRect: Rect => String => Canvas => Canvas, content: Iterable[_]): CustomDrawable = {
    CustomDrawable(rect, drawWithRect, content.toString)
  }

  def apply(value: Int, rect: Rect): CustomDrawable = {
    CustomDrawable(rect, StandaloneDrawing.pprintDrawingWithBox, value.toString, value)
  }
}
