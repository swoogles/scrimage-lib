package imagemanipulation

import java.awt.{Color => JColor}
import com.sksamuel.scrimage.canvas.drawable.Rect
import java.awt.Graphics2D

sealed trait Column {
  val value: Int
}
sealed trait Row {
  val value: Int
}
object Columns {
  object COL_0 extends Column { val value = 0 }
  object COL_1 extends Column { val value = 1 }
  object COL_2 extends Column { val value = 2 }
  object COL_3 extends Column { val value = 3 }
  object COL_4 extends Column { val value = 4 }
  object COL_5 extends Column { val value = 5 }
  object COL_6 extends Column { val value = 6 }
  object COL_7 extends Column { val value = 7 }
  object COL_8 extends Column { val value = 8 }
  object COL_9 extends Column { val value = 9 }
  object COL_10 extends Column { val value = 10 }
  object COL_11 extends Column { val value = 11 }
}

object Rows {
  object ROW_0 extends Row { val value = 0 }
  object ROW_1 extends Row { val value = 1 }
  object ROW_2 extends Row { val value = 2 }
  object ROW_3 extends Row { val value = 3 }
  object ROW_4 extends Row { val value = 4 }
  object ROW_5 extends Row { val value = 5 }
  object ROW_6 extends Row { val value = 6 }
  object ROW_7 extends Row { val value = 7 }
  object ROW_8 extends Row { val value = 8 }
  object ROW_9 extends Row { val value = 9 }
  object ROW_10 extends Row { val value = 10 }
  object ROW_11 extends Row { val value = 11 }
}

class ScaledBoxes(width: Int, height: Int) extends TextDrawing {
  protected val rectangleConfig = { g2: Graphics2D =>
      g2.setColor(JColor.GREEN)
      g2.setFont(imgFont)
    }

  // private def smallRectangleAt(x: Int, y: Int) = Rect(x=x, y=y, width=(width*(1/16.0)).toInt, (width*(1/16.0)).toInt, rectangleConfig )
  val smallRectangleWidth = (width*(1/16.0)).toInt
  val wideRectangleWidth = smallRectangleWidth*3

  def smallRectangleAt(col: Column, row: Row) = {
    Rect(
      x=smallRectangleWidth*col.value,
      y=(height*(row.value/12.0)).toInt,
      width=(width*(1/16.0)).toInt,
      (width*(1/16.0)).toInt, rectangleConfig 
    )
  }

  def wideRectangleAt(col: Column, row: Row): Rect =
    smallRectangleAt(col,row)
      .copy(width=wideRectangleWidth)

}

