package imagemanipulation

import java.awt.{Color => JColor, Font => JFont, Image => JImage}
import com.sksamuel.scrimage.canvas.drawable.Rect
import com.sksamuel.scrimage.canvas.drawable.Text

trait CustomDrawable {
  val rect: Rect
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

}

case class ImgDrawable(rect: Rect, imgFile: java.io.File) extends CustomDrawable {
  import com.sksamuel.scrimage.Image
  import com.sksamuel.scrimage.ScaleMethod.FastScale
  import com.sksamuel.scrimage.canvas.Canvas
  val image1 = Image.fromFile(imgFile)
    .scaleTo(rect.width,rect.height, FastScale)

    def draw(canvas: Canvas) = {
      canvas.draw(rect).overlay(image1, rect.x, rect.y)
    }

  def onNextRow = {
    val nextRect = rect.copy(y=rect.y+75)
    copy(rect = nextRect)
  }

  def nextStageOpt(img: java.io.File) = {
    val rand = new scala.util.Random
    if( scala.math.abs(scala.util.Random.nextInt % 100) > 20 ) {
      Some(onNextRow.copy(imgFile = img))
    } else None
  }

  def nextStageList(img: java.io.File) = {
    List.fill(3)(onNextRow.copy(imgFile = img))
  }

}

object CustomDrawable {
  def spaceRow( imgItems: List[CustomDrawable] ): List[Rect] = {
    val (head :: tail) = imgItems
    val x = head.rect.x
    val y = head.rect.y
    val (finalRect, spacedList: List[Rect]) = tail.fold((head.rect, List(head.rect))) { case ((lastRect: Rect, accItems: List[Rect]), nextItem: CustomDrawable) =>
      val newRect = nextItem.rect.copy(x = lastRect.x + lastRect.width + 10)
      (newRect, accItems :+ newRect)

    }
    spacedList

  }

}

sealed trait LongItem extends TextDrawable {
  override val imgFont = new JFont("Sans-seriff", 1, 14)
}

case class NumericalListItem(rect: Rect, value: Int = 1) extends TextDrawable {
  val content = value.toString

  def onNextRow = {
    val nextRect = rect.copy(y=rect.y+75)
    copy(rect = nextRect)
  }

}

case class ListItem(rect: Rect, label: Char, value: Int = 1) extends TextDrawable {
  val content = label.toString
  override val text =
       Text(label.toString, rect.x+15, rect.y+30, { g2 =>
         g2.setColor(JColor.BLUE)
         g2.setBackground(JColor.WHITE)
         g2.setFont(imgFont)
       })
}

case class PhoneNumberListItem(phoneNumber: String, rect: Rect, value: Int = 1) extends TextDrawable {
  override val imgFont = new JFont("Sans-seriff", 1, 14)
  val content = phoneNumber
}

