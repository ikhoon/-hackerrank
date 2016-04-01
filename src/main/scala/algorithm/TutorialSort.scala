package algorithm

/**
  * Created by ikhoon on 2016. 3. 23..
  */
object TutorialSort extends App {

  val input = io.Source.stdin.getLines()
  val v = input.next().toInt
  val n = input.next().toInt
  val ar = input.next().split(" ").map(_.toInt)
  println(ar.indexOf(v))

}
