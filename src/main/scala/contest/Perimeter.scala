package contest

/**
  * Created by ikhoon on 2016. 3. 26..
  */
object Perimeter extends App {
  val input = io.Source.stdin.getLines()

  val n = input.next().trim.toInt
  val (x, y) = getXY(input.next().trim)
  val r = (2 to n).foldLeft((0D, (x, y))) {
    case ((acc, (px, py)), i) =>
      val (cx, cy) = getXY(input.next().trim)
      val dist = distance(px, py, cx, cy)
      (acc + dist, (cx, cy))
  }
  val last = distance(x, y, r._2._1, r._2._2)
  println(r._1 + last)


  def distance(x1: Double, y1: Double, x2: Double, y2: Double): Double =
    math.sqrt(math.pow(x1 - x2, 2) + math.pow(y1 - y2, 2))

  def getXY(str: String):(Int, Int) = {
    val xy = str.trim.split(" ")
    val x = xy.head.toInt
    val y = xy.last.toInt
    (x, y)
  }
}
