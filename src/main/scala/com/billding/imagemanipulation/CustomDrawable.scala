package imagemanipulation

import java.awt.{Color => JColor, Font => JFont, Image => JImage}
import com.sksamuel.scrimage.canvas.drawable.Rect
import com.sksamuel.scrimage.canvas.drawable.Text

sealed trait CustomDrawable {
  val rect: Rect
  val value: Int
  val content: String
  val imgFont = new JFont("Sans-seriff", 1, 28)

  def text: Text =
       Text(content.toString, rect.x+15, rect.y+30, { g2 =>
         g2.setBackground(JColor.BLUE)
         g2.setFont(imgFont)
       })

}

case class ImgDrawable(rect: Rect, imgFile: java.io.File) {
  import com.sksamuel.scrimage.Image
  import com.sksamuel.scrimage.ScaleMethod.FastScale
  import com.sksamuel.scrimage.canvas.Canvas
  val image1 = Image.fromFile(imgFile)
    .scaleTo(rect.width,rect.height, FastScale)
    def draw(canvas: Canvas) = {
      canvas.draw(rect).overlay(image1, rect.x, rect.y)

    }


}

sealed trait LongItem extends CustomDrawable {
  override val imgFont = new JFont("Sans-seriff", 1, 14)
}

case class NumericalListItem(rect: Rect, value: Int = 1) extends CustomDrawable {
  val content = value.toString
}

case class ListItem(rect: Rect, label: Char, value: Int = 1) extends CustomDrawable {
  val content = label.toString
  override val text =
       Text(label.toString, rect.x+15, rect.y+30, { g2 =>
         g2.setColor(JColor.BLUE)
         g2.setBackground(JColor.WHITE)
         g2.setFont(imgFont)
       })
}

case class PhoneNumberListItem(phoneNumber: String, rect: Rect, value: Int = 1) extends CustomDrawable {
  override val imgFont = new JFont("Sans-seriff", 1, 14)
  val content = phoneNumber
}

