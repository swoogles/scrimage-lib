package imagemanipulation

import org.scalatest.FlatSpec

class ScrimageFunTests extends FlatSpec {

  "scrimage" should "create images for every step" in {
    ScrimageFun.drawSomeSquares()
    println("pointless test")
  }

}
