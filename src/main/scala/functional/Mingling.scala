package functional

/**
  * Created by ikhoon on 2016. 3. 21..
  */
object Mingling extends App {

  val input = io.Source.stdin.getLines()
  val p: String = input.next()
  val q: String = input.next()
  (0 until p.length).foreach {
    i => print(p.charAt(i).toString + q.charAt(i).toString)
  }

}
