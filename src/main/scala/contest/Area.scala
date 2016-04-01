package contest

/**
  * Created by ikhoon on 2016. 3. 26..
  */
object Area extends App {

  def calc(x1: Double, y1: Double, x2: Double, y2: Double): Double =
    (x1 * y2 - y1 * x2) / 2

  def getXY(str: String):(Int, Int) = {
    val xy = str.trim.split(" ")
    val x = xy.head.toInt
    val y = xy.last.toInt
    (x, y)
  }

  val input = io.Source.stdin.getLines()
  val n = input.next().trim.toInt
  val (x, y) = getXY(input.next().trim)
  val r = (2 to n).foldLeft((0D, (x, y))) {
    case ((acc, (px, py)), i) =>
      val (cx, cy) = getXY(input.next().trim)
      val area = calc(px, py, cx, cy)
      (acc + area, (cx, cy))
  }
  val last = calc(r._2._1, r._2._2, x, y)
  println(math.abs(r._1 + last))


}
