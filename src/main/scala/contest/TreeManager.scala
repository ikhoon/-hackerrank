package contest



/**
  * Created by ikhoon on 2016. 3. 26..
  */
object TreeManager extends App {

  class Node(val parent: Node, var n: Int, var child: List[Node] = List[Node]())

  val root : Node = new Node(null, 0)
  var current : Node = root

  def print() = println(current.n)
  // change
  def changeValue(n: Int) = current.n = n

  // visit
  def visitLeft() = {
    val siblings = current.parent.child
    current = siblings.take(siblings.indexOf(current)).last
  }
  def visitRight() = {
    val siblings = current.parent.child
    current = siblings.drop(siblings.indexOf(current) + 1).head
  }

  def visitParent() = current = current.parent
  def visitChild(n : Int) = current = current.child(n - 1)

  // insert
  def insertLeft(n : Int) = {
    val newNode = new Node(current.parent, n)
    val siblings = current.parent.child
    val pos = siblings.indexOf(current)
    current.parent.child = siblings.take(pos) ::: List(newNode) ::: siblings.drop(pos)
  }
  def insertRight(n : Int) = {
    val newNode = new Node(current.parent, n)
    val siblings = current.parent.child
    val pos = siblings.indexOf(current)
    current.parent.child = siblings.take(pos + 1) ::: List(newNode) ::: siblings.drop(pos + 1)
  }

  def insertChild(n : Int) = {
    val newChild = new Node(current, n)
    current.child = newChild :: current.child
  }

  // delete
  def delete() = {
    val siblings = current.parent.child
    val pos = siblings.indexOf(current)
    current.parent.child = siblings.take(pos) ::: siblings.drop(pos + 1)
    current = current.parent
  }


  object Commands {
    val ChangeValue = "change (\\d+)"r
    val Delete = "delete"
    val Print = "print"
    val InsertN = "insert (left|right|child) (\\d+)"r
    val Visit = "visit (left|right|parent)"r
    val VisitChildN = "visit child (\\d+)"r
  }

  object Actions {
    import Commands._

    def run(command: String) = command match {
      case ChangeValue(n) => changeValue(n.toInt)
      case Delete => delete()
      case Print => print()
      case InsertN(direction, n) => direction match {
        case "left" => insertLeft(n.toInt)
        case "right" => insertRight(n.toInt)
        case "child" => insertChild(n.toInt)
      }
      case Visit(direction) => direction match {
        case "left" => visitLeft()
        case "right" => visitRight()
        case "parent" => visitParent()
      }
      case VisitChildN(n) => visitChild(n.toInt)
    }
  }

  val input = io.Source.stdin.getLines()
  val numOfCommands = input.next().trim.toInt
  (1 to numOfCommands).foreach { _ => Actions.run(input.next().trim) }

}
