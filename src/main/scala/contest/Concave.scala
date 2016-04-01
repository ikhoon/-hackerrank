package contest


/**
  * Created by ikhoon on 2016. 3. 26..
  */
object Concave extends App {

  case class Point(x: Double, y: Double)

  case class Vec(x: Double, y: Double) {
    def angle(other: Vec):Double = math.atan2(cross(other), dot(other))
    def angle2(other: Vec):Double = {
      val ag = math.atan2(y * other.x - x * other.y, x * other.x + y * other.y)
      if(ag < 0) ag + 2 * math.Pi else ag
    }
    def dot(other: Vec): Double = x * other.x + y * other.y
    def cross(other: Vec): Double = x * other.y - y * other.x
  }

  def getXY(str: String):Point = {
    val xy = str.trim.split(" ")
    Point(xy.head.toInt, xy.last.toInt)
  }

  def shift[T](list: Seq[T]): Seq[T]= list.tail ++ Seq(list.head)

  def getAngle[T <: Point](i: Int, list: Seq[T]): Double = {
    val size = list.size
    val a = list(i % size)
    val b = list((i + 1) % size)
    val c = list((i + 2) % size)
    getAngleP(a, b, c)
  }

  def getAngleP(a: Point, b: Point, c: Point): Double = {
    val vba = Vec(a.x - b.x, a.y - b.y)
    val vbc = Vec(c.x - b.x, c.y - b.y)
    vba.angle2(vbc)
  }

  def getAngleV(v1: Vec, v2: Vec): Double = {
    math.atan2(v2.y, v2.x) - math.atan2(v1.y, v1.x)
  }

  val input = io.Source.stdin.getLines()
  val n = input.next().trim.toInt
  val polygonA = (1 to n).map{ i => getXY(input.next().trim) }
  if(n <= 3) println("NO")
  else {
    val expected = (n - 2) * math.Pi
    val permutations = polygonA.permutations.toStream
    val result = permutations.dropWhile(polygon => {
      val count = (0 until n).map(i => getAngle(i, polygon)).count(_ > math.Pi)
      count == 0
    }).headOption

    println(if(result.isDefined) "NO" else "YES")
  }
}
