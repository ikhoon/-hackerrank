package functional

import scala.io.Source.stdin

/**
  * Created by ikhoon on 2016. 3. 21..
  */
object Pascal extends App{
  def f(): Unit = {
    val n = stdin.getLines().next().toInt
    pascal(Nil, n)
  }


  def pascal(prev: List[Int], until: Int): Unit = {
    if (until == 0) ()
    else {
      val now = if (prev.isEmpty) 1 :: prev else (0 to prev.size).map(number(_, prev)).toList
      println(now.mkString(" "))
      pascal(now, until - 1)
    }
  }
  def number(pos : Int, prev: List[Int]) : Int = {
    val left = if(prev.isDefinedAt(pos - 1)) prev(pos - 1) else 0
    val right = if(prev.isDefinedAt(pos)) prev(pos) else 0
    left + right
  }

  f()
}
