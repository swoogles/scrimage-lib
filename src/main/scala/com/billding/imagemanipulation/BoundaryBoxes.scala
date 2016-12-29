package imagemanipulation

import java.awt.{Color => JColor}
import com.sksamuel.scrimage.canvas.drawable.Rect
import java.awt.Graphics2D

trait BoundaryBoxes extends TextDrawing {
  protected val rectangleConfig = { g2: Graphics2D =>
      g2.setColor(JColor.GREEN)
      g2.setFont(imgFont)
    }

  protected def smallRectangleAt(x: Int, y: Int) = Rect(x=x, y=y, width=50, height=50, rectangleConfig )

  protected def wideRectangleAt(x: Int, y: Int) = Rect(x=x, y=y, width=150, height=50, rectangleConfig )

}

class ScaledBoxes(width: Int, height: Int) extends TextDrawing {
  val colWidth = width / 8
  val rowHeight = height / 8
  sealed trait Column {
    val value: Int
  }
  sealed trait Row {
    val value: Int
  }
  object COL_0 extends Column { val value = colWidth * 0 }
  object COL_1 extends Column { val value = colWidth * 1 }
  object COL_2 extends Column { val value = colWidth * 2 }
  object COL_3 extends Column { val value = colWidth * 3 }
  object COL_4 extends Column { val value = colWidth * 4 }
  object COL_5 extends Column { val value = colWidth * 5 }
  object COL_6 extends Column { val value = colWidth * 6 }
  object COL_7 extends Column { val value = colWidth * 7 }

  object ROW_0 extends Row { val value = rowHeight * 0 }
  object ROW_1 extends Row { val value = rowHeight * 1 }
  object ROW_2 extends Row { val value = rowHeight * 2 }
  object ROW_3 extends Row { val value = rowHeight * 3 }
  object ROW_4 extends Row { val value = rowHeight * 4 }
  object ROW_5 extends Row { val value = rowHeight * 5 }
  object ROW_6 extends Row { val value = rowHeight * 6 }
  object ROW_7 extends Row { val value = rowHeight * 7 }
  protected val rectangleConfig = { g2: Graphics2D =>
      g2.setColor(JColor.GREEN)
      g2.setFont(imgFont)
    }

  def smallRectangleAt(x: Int, y: Int) = Rect(x=x, y=y, width=(width*(1/16.0)).toInt, (width*(1/16.0)).toInt, rectangleConfig )
  def smallRectangleAt(col: Column, row: Row) = Rect(x=col.value, y=row.value, width=(width*(1/16.0)).toInt, (width*(1/16.0)).toInt, rectangleConfig )

  def wideRectangleAt(x: Int, y: Int) = Rect(x=x, y=y, width=(width*(3/16.0)).toInt, (width*(1/16.0)).toInt, rectangleConfig )

}

