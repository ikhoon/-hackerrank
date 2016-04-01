package algorithm

import scala.collection.mutable.ArrayBuffer
import scala.collection.parallel.mutable.ParArray

/**
  * Created by ikhoon on 2016. 3. 23..
  */
object MaxsubArray extends App {

  val input = io.Source.stdin.getLines()
  val t = input.next().trim.toInt
  (1 to t).foreach { _ =>
    val n = input.next().trim.toInt
    val ar = input.next().trim.split(" ").par.map(_.toInt)
    var left: Int = 0
    var right: Int = 0

    (0 until n).foreach {
      case 0 => Unit
      case i =>
        val (l, r) = update(i, left, right, ar)
        left = l
        right = r
    }
    val maxContinuous = ar.slice(left, right + 1).sum
    val maxNonContinuous = if (maxContinuous <= 0) maxContinuous else ar.filter(_ > 0).sum
    println(s"$maxContinuous $maxNonContinuous")
  }

  def update(i : Int, l: Int, r: Int, ar: ParArray[Int]): (Int, Int)= {
    val current = ar(i)
    if(current < 0) {
      if (current > ar.slice(l, r + 1).sum) {
        (i, i)
      } else {
        (l, r)
      }
    }
    else {
      if(i - 1 == r) {
        (l, i)
      }
      else {
        if(ar.slice(r + 1, i + 1).sum < 0) {
          (l, r)
        }
        else {
          if(ar.slice(l, i + 1).sum > current) {
            (l, i)
          }
          else {
            (i, i)
          }
        }
      }

    }
  }

}
