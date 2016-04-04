package functional


/**
  * Created by ikhoon on 2016. 4. 4..
  */
object SierpinskiTriangles extends App {


  import scala.collection.mutable
  val matrix  = mutable.Buffer.fill(32, 63)('_')
  def drawTriangles(n : Int) = {
    val x = matrix.head.size / 2
    val y = matrix.size
    drawTriangle((x, 0), y, n)
    draw(matrix)
  }

  def drawTriangle(start: (Int, Int), height: Int, n: Int) = {
    (0 until height).foreach(y => {
      matrix(start._2 + y)(start._1) = '1'
      (1 to y).foreach(x => {
        matrix(start._2 + y)(start._1 + x) = '1'
        matrix(start._2 + y)(start._1 - x) = '1'
      })
    })
    val nextStart = (start._1, height  / 2)
    if (n > 0) drawReverseTriangle(nextStart, height / 2, n - 1)
  }

  def drawReverseTriangle(start: (Int, Int), height: Int, n: Int) : Unit = {
    (0 until height).foreach(y => {
      matrix(start._2 + y)(start._1) = '_'
      (1 until height - y).foreach(x => {
        matrix(start._2 + y)(start._1 + x) = '_'
        matrix(start._2 + y)(start._1 - x) = '_'
      })
    })
    if(n > 0) {
      val nextHeight = height / 2
      // left
      val leftStart = (start._1 - height, start._2 + nextHeight)
      drawReverseTriangle(leftStart, nextHeight, n - 1)
      // right
      val rightStart = (start._1 + height, start._2 + nextHeight)
      drawReverseTriangle(rightStart, nextHeight, n - 1)
      // up
      val upStart = (start._1, start._2 - nextHeight)
      drawReverseTriangle(upStart, nextHeight, n - 1)
    }
  }
  def draw(matrix: mutable.Buffer[mutable.Buffer[Char]]) = {
    matrix.foreach(row => {
      row.foreach({ col =>
        print(col)
      })
      println()
    })
  }

  drawTriangles(5)
}
