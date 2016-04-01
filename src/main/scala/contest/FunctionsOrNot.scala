package contest


/**
  * Created by ikhoon on 2016. 3. 25..
  */
object FunctionsOrNot extends App {

  val input = io.Source.stdin.getLines()
  val t = input.next().trim.toInt
  (1 to t).foreach {
    i => {
      val n = input.next().trim.toInt
      val r = (1 to n).foldLeft((true, Map[Int, Int]())){
        case ((isFunction, f), elem) =>
          val xy = input.next().trim.split(" ")
          if(!isFunction) (false, f)
          else {
            val x = xy.head.toInt
            val y = xy.last.toInt
            val fy: Option[Int] = f.get(x)
            fy match {
              case None => (true, f + (x -> y))
              case Some(`y`) => (true, f)
              case Some(_) => (false, f)
            }
          }
      }
      println(if(r._1) "YES" else "NO")
    }
  }
}
