val str = "-123"
val number = """(-?[0-9]+)""".r
str match {
  case number(n) => println(n)
}