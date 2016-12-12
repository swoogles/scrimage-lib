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

  def nextStageOpt(img: java.io.File) = {
    val rand = new scala.util.Random
    if( scala.math.abs(scala.util.Random.nextInt % 100) > 20 ) {
      val nextRect = rect.copy(y=rect.y+75)
      Some(copy(rect = nextRect, imgFile = img))
    } else None
  }

  def nextStageList(img: java.io.File) = {
    val rand = new scala.util.Random
    val nextRect = rect.copy(y=rect.y+75)
    List.fill(3)(copy(rect = nextRect, imgFile = img))
  }

}

object ImgDrawable {
  def spaceRow( imgItems: List[ImgDrawable] ): List[ImgDrawable] = {
    val (head :: tail) = imgItems
    val x = head.rect.x
    val y = head.rect.y
    val (finalRect, spacedList: List[ImgDrawable]) = tail.fold((head.rect, List(head): List[ImgDrawable])) { case ((lastRect: Rect, accItems: List[_]), nextItem: ImgDrawable) =>
      val newRect = nextItem.rect.copy(x = lastRect.x + lastRect.width + 10)
      val itemWithUpdatedPos = nextItem.copy(rect=newRect)
      // nextItem.
      (newRect, accItems :+ itemWithUpdatedPos)

      case other => 
        println("Unrecognized line:")
        println(other)
        other
    }
    spacedList

  }
}

object CustomDrawable {
  def spaceRow( imgItems: List[CustomDrawable] ): List[Rect] = {
    val (head :: tail) = imgItems
    val x = head.rect.x
    val y = head.rect.y
    val (finalRect, spacedList: List[Rect]) = tail.fold((head.rect, List(head.rect): List[Rect])) { case ((lastRect: Rect, accItems: List[_]), nextItem: CustomDrawable) =>
      val newRect = nextItem.rect.copy(x = lastRect.x + lastRect.width + 10)
      // val itemWithUpdatedPos = nextItem.copy(rect=newRect)
      // nextItem.
      (newRect, accItems :+ newRect)

      case other => 
        println("Unrecognized line:")
        println(other)
        other
    }
    spacedList

  }
}

sealed trait LongItem extends TextDrawable {
  override val imgFont = new JFont("Sans-seriff", 1, 14)
}

case class NumericalListItem(rect: Rect, value: Int = 1) extends TextDrawable {
  val content = value.toString
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

