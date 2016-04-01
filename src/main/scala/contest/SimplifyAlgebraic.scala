package contest

import scala.io.Source

/**
  * Created by ikhoon on 2016. 3. 27..
  */
object SimplifyAlgebraic extends App {
  trait Expr { self =>
    def /(r: Expr): Expr = Div(self, r)
    def +(r: Expr): Expr = Plus(self, r)
    def -(r: Expr): Expr = Plus(self, r * Algebra(-1))
    def *(r: Expr): Expr = Prod(self, r)
    def eval: Expr
    def descending: Expr
    def flatten: Expr
  }
  trait Op {
    def left: Expr
    def right: Expr
    def left_=(expr: Expr): Unit
    def right_=(expr: Expr): Unit
  }


  case class Algebra(c: Int, p: Int = 0) extends Expr { self =>

    override def +(r: Expr): Expr = r match {
      case r: Algebra =>
        if(p == r.p) copy(c = c + r.c)
        else if (p > r.p) Plus(self, r)
        else Plus(r, self)
      case Plus(left, right: Plus) => Plus(left + r, right)
      case _ => super.+(r)
    }

    override def -(r: Expr): Expr = r match {
      case r: Algebra =>
        if(p == r.p) copy(c = c - r.c)
        else if (p > r.p) Plus(self, r.copy(-c))
        else Plus(r.copy(-c), self)
      case Plus(left, right: Plus) => Plus(left - r, right)
      case _ => super.-(r)
    }

    override def *(r: Expr): Expr = r match {
      case r: Algebra => copy(c * r.c, p + r.p)
      case Plus(left, right : Plus) => Plus(self * left, self * right)
      case _ => super.*(r)
    }

    override def /(r: Expr): Expr = r match {
      case r: Algebra => copy(c / r.c, p -r.p)
      case Plus(left, right : Plus) => Plus(self / left, self)
      case _ => super./(r)
    }


   override def toString: String = {
     if(c == 0) ""
     else {
       val cstr = if (c == 1) "" else if (c == -1) "-" else c.toString
       if (p == 0) cstr
       else if (p == 1) s"${cstr}x"
       else s"${cstr}x^$p"
     }
    }
    override def eval: Expr = self

    override def descending: Expr = self

    override def flatten: Expr = self
  }


  case class Div(var left: Expr, var right: Expr) extends Expr with Op{ self =>
    override def eval: Expr = self
    override def flatten: Expr = left match {
      case Plus(l, r) => Plus(Div(l, right).flatten, Div(r, right).flatten).flatten
      case p@Prod(l, r) => Div(p.flatten, right.flatten).flatten
      case d@Div(l, r) => Div(d.flatten, right.flatten).flatten
      case a@Algebra(c, p) => right.flatten match {
        case Algebra(rc, rp) => Algebra(c / rc, p - rp)
        case Div(l, r) => Div(l, Div(r, a).flatten).flatten
        case x => Div(x, a).flatten
      }
    }

    override def descending: Expr = this
  }
  case class Prod(var left: Expr, var right: Expr) extends Expr with Op { self =>
    // TODO
    override def flatten: Expr = left match {
      case Plus(l, r) => right.flatten match {
        case Plus(ll, rr) => Plus(Prod(l, ll), Plus(Prod(l, rr), Plus(Prod(r, ll), Plus(r, rr)))).flatten
        case _ => Plus(Prod(l, right).flatten, Prod(r, right).flatten).flatten
      }
      case Plus(l, r) => Plus(Prod(l, right).flatten, Prod(r, right).flatten).flatten
      case p@Prod(l, r) => Prod(p.flatten, right.flatten).flatten
      case d@Div(l, r) => Prod(d.flatten, right.flatten).flatten
      case a@Algebra(c, p) => right.flatten match {
        case Algebra(rc, rp) => Algebra(c * rc, p + rp)
        case Prod(l, r) => Prod(Prod(l, a).flatten, Prod(r, a).flatten).flatten
        case Plus(l, r) => Plus(Prod(l, a).flatten, Prod(r, a).flatten).flatten
        case x => Prod(x, a).flatten
      }
    }
    override def descending :Expr = left.descending
    override def eval: Expr = left.eval
  }

  case class Plus(var left: Expr, var right: Expr) extends Expr with Op { self =>
    override def flatten : Expr = {
      (left, right) match {
        case (x1 : Algebra, x2: Algebra) => x1 + x2
        case (x1 : Algebra, op1) => Plus(x1, op1.flatten)
        case (x1 : Algebra, op1) => Plus(x1, op1.flatten)
        case (op1: Plus, x1: Algebra) => Plus(x1, op1.flatten)
        case (op1 : Plus, op2: Plus) =>
          val pivot = op1.right
          op1.right = self
          self.left = pivot
          op1.flatten
          op2.flatten
        case (op1, op2) => Plus(op1.flatten, op2.flatten)
        // 이걸 algebra, op의 형태로 변경해야한다
      }
    }

    override def descending : Expr = (left, right) match {
      case (x1 : Algebra, op @ Plus(x2 : Algebra, x3)) =>
        if (x1.p >= x2.p) {
          self.right = op.descending
          self.right match {
            case Plus(rl : Algebra, rr)  => if(x1.p < rl.p) self.descending else self
          }
        }
        else {
          self.left = x2
          op.left = x1
          self.right = op.descending
          self.right match {
            case Plus(rl : Algebra, rr)  => if(x2.p < rl.p) self.descending else self
          }
        }
      case (op1 : Plus, op2 : Algebra) =>
        self.left = op2
        self.right = op1.descending
        self.descending
      case (op1 : Plus, op2: Plus) =>
        val pivot = op1.right
        self.left = pivot
        op1.right = self
        op1.descending
      case (x1 : Algebra, x2 : Algebra) => if (x1.p >= x2.p) self else {
        self.left = x2
        self.right = x1
        self
      }
      case _ => self
    }


    def eval : Expr = {
      // tree를 만들자 한뱡향으로 치우친
      (left, right) match {
        case (x1 : Algebra, x2: Algebra) => x1 + x2
        case (x1 : Algebra, op1 @ Plus(x2 : Algebra, x3)) =>
          x1 + x2 match {
            case sum : Algebra => Plus(sum, x3).eval
            case Plus(n1, n2) => Plus(n1, Plus(n2, x3).eval)
          }
        case _ => self
      }
    }

    override def toString: String = {
      val leftStr = left.toString
      val rightStr = right.toString
      if(leftStr.isEmpty) rightStr
      else if(rightStr.isEmpty) leftStr
      else if(rightStr.startsWith("-")) s"$leftStr - ${rightStr.drop(1)}"
      else s"$leftStr + $rightStr"
    }
  }


  object AlgebraParser extends util.parsing.combinator.RegexParsers{

    private def cxx : Parser[Algebra] = """-?[0-9]+x\^[0-9]+""".r ^^ { case cx =>
        val t= cx.split("x\\^")
        Algebra(t.head.toInt, t.last.toInt)
    }
    private def cx : Parser[Algebra] = """(-?[0-9]+)x""".r ^^ { c => Algebra(c.init.toInt, 1) }
    private def c  : Parser[Algebra] = """(-?[0-9]+)""".r ^^ { c => Algebra(c.toInt, 0) }
    private def xx : Parser[Algebra] = """x\^[0-9]+""".r ^^ { case cx => Algebra(1, cx.drop(2).toInt) }
    private def x  : Parser[Algebra] = """x""".r ^^ { c => Algebra(1, 1) }
    private def op : Parser[String] = "*" | "/" | "+" | "-"

    private def algebra : Parser[Algebra] = cxx | xx | cx | x | c

    private def expr: Parser[Expr] = (group | algebra) ~ rep(op ~ (group | algebra)) ^^ {
      case l ~ xs => expression(l, xs)
    }
    private def group : Parser[Expr] = "(" ~> expr <~ ")" ^^ { case ex => Prod(ex, Algebra(1)) }

    private def expression(l: Expr, xs: List[~[String, Expr]]) : Expr = xs match {
      case op ~ r :: rest => expression(operate(l, op, r), rest)
      case _ => l
    }

    private def operate(l: Expr, op: String, r: Expr) : Expr = op match {
      case "+" => Plus(l, r)
      case "-" => Plus(l, r * Algebra(-1))
      case "*" => insertRightMost(l, op, r)
      case "/" => insertRightMost(l, op, r)
    }

    def insertRightMost(root: Expr, op: String, r: Expr) : Expr = {
      root match {
        case parent@Plus(left, right) =>
          subInsertRightMost(parent, right, op, r); root
        case _ => op match {
          case "*" => Prod(root, r)
          case "/" => Div(root, r)
        }
      }
    }
    def subInsertRightMost(parent: Expr with Op, current: Expr, op: String, r: Expr) : Unit = {
      current match {
        case c@Plus(left, right) =>
          subInsertRightMost(c, right, op, r)
        case _ => op match {
          case "*" => parent.right = Prod(current, r)
          case "/" => parent.right = Div(current, r)
        }
      }
    }

    def build(str: String): Expr = parse(expr, str).get
  }

  val input = io.Source.stdin.getLines()
  val n = input.next().trim.toInt
  (1 to n).foreach {
    i => println(AlgebraParser.build(input.next().trim).flatten.descending.eval)
  }

}
