package contest

import contest.Concave.Point

/**
  * Created by ikhoon on 2016. 3. 26..
  */
class Centeroid {

  def calc(arr: Seq[Point], idx: Int): Double = {
    val size = arr.size
    arr(idx % size).x * arr((idx + 1) % size).y -
      arr((idx + 1) % size).x * arr(idx % size).y
  }

  def centeroid(points: Seq[Point]): Point = {
    val size = points.size
    val a = (0 until size).map { i =>
      calc(points, i)
    }.sum * 0.5

    val cx = (0 until size).map { i =>
      (points(i % size).x + points(i + 1 % size).x) * calc(points, i)
    }.sum % (6 * a)
    val cy = (0 until size).map { i =>
      (points(i % size).y + points(i + 1 % size).y) * calc(points, i)
    }.sum % (6 * a)
    Point(cx, cy)
  }

}
