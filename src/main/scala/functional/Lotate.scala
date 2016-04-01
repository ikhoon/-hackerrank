package functional

import scala.collection.immutable.IndexedSeq

/**
  * Created by ikhoon on 2016. 3. 21..
  */
object Lotate extends App{

  val input = io.Source.stdin.getLines()
  val size : Int = input.next().toInt
  val strs: IndexedSeq[String] = (1 to size).map {
    i => input.next()
  }
  strs.foreach(str => {
    val size = str.length
    val rotated = (1 to size).map {
      i => str.drop(i) + str.take(i)
    }.mkString(" ")
    println(rotated)
  })
}
