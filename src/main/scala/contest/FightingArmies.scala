package contest
import scala.collection.mutable

/**
  * Created by ikhoon on 2016. 3. 27..
  */
object FightingArmies extends App {

  def findStrongest(i : Int) = println(N(i - 1).max)
  def strongestDied(i : Int) = {
    val army = N(i - 1)
    army.remove(army.indexOf(army.max))
  }
  def recruit(i: Int, c: Int) = {
    val army = N(i - 1)
    army += c
  }
  def merge(i: Int, j: Int) = {
    val armyI = N(i - 1)
    val armyJ = N(j - 1)
    armyI ++= armyJ
    armyJ.clear()
  }

  object Commands {
    val FindStrongest = "1 (\\d+)"r
    val StrongestDied = "2 (\\d+)"r
    val Recruit = "3 (\\d+) (\\d+)"r
    val Merge = "4 (\\d+) (\\d+)"r
  }

  object Actions {
    import Commands._

    def run(command: String) = command match {
      case FindStrongest(i) => findStrongest(i.toInt)
      case StrongestDied(i) => strongestDied(i.toInt)
      case Recruit(i, c) => recruit(i.toInt, c.toInt)
      case Merge(i, j) => merge(i.toInt, j.toInt)
    }
  }

  val input = io.Source.stdin.getLines()
  val header = input.next().trim.split(" ")
  val n = header.head.toInt
  val q = header.last.toInt

  val N = (1 to n).map(_ => mutable.ArrayBuffer[Int]())
  (1 to q).foreach(_ => Actions.run(input.next().trim))
}
