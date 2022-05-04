//@main
//def main(): Unit = {
//  import scala.io.StdIn._
//  val name = readLine("Enter your name: ")
//  println("Enter your age: ")
//  val age = readInt()
//  println(Console.BOLD)
//  print("Name: ")
//  print(Console.UNDERLINED)
//  print(name)
//  println(Console.BOLD)
//  print("Age: ")
//  print(Console.RESET)
//  print(age)
//}

/**
 * @see http://en.wikipedia.org/wiki/Reverse_Polish_notation
 * @see http://algolist.manual.ru/syntax/revpn.php
 */
object Eval extends App {
  val ops = List("(", ")", "+", "-", "*", "/", "^")
  val precedence = Map("-" -> 2, "+" -> 2, "/" -> 3, "*" -> 3, "^" -> 4)
  def associativity(op: String): String = if (op == "^") "right" else "left"
  def parenthesis(x: String): Boolean = x == "(" || x == ")"
  def operator(x: String): Boolean = ops.contains(x) && !parenthesis(x)
  def number(x: String): Boolean = !ops.contains(x)

  def eval(expr: String): Either[String, Double] = {
    val infix = split(ops: _*)(expr)
    val postfix = toPostfix(infix)
    convert(postfix).foldLeft(List.empty[Double]) {
      case (stack, token) =>
        token match {
          case n: Double => n :: stack
          case op => stack match {
            case x1 :: x2 :: xs =>
              val result = op match {
                case "+" => x2 + x1
                case "-" => x2 - x1
                case "*" => x2 * x1
                case "/" =>
                  if (x1 == 0) return Left("Divide by zero")
                  else x2 / x1
                case "^" => math.pow(x2, x1)
              }
              result :: xs
            case _ => return Left("Stack contains less than 2 elements")
          }
        }
    } match {
      case x :: Nil => Right(x)
      case _ => Left("Stack must contain one element at the end")
    }
  }

  def convert(tokens: List[String]): List[Any] = tokens.map {
    case x if ops.contains(x) => x
    case x => x.toDouble
  }

  def toPostfix(infix: List[String]): List[String] = {
    val (stack, out) = infix.foldLeft((List.empty[String], List.empty[String])) {
      case ((stack, acc), token) =>
        token match {
          case n if number(n) => (stack, acc :+ n)
          case op if operator(op) =>
            val (head, tail) = associativity(op) match {
              case "left" => stack.span(x => operator(x) && precedence(token) <= precedence(x))
              case "right" => stack.span(x => operator(x) && precedence(token) < precedence(x))
            }
            (op :: tail, acc ++ head)
          case "(" => (token :: stack, acc)
          case ")" =>
            val (head, tail) = stack.span(_ != "(")
            (tail.tail, acc ++ head)
        }
    }
    out ++ stack
  }

  def split(separators: String*)(expr: String): List[String] = {
    expr.filterNot(_ == ' ').map(String.valueOf).foldLeft(List.empty[String]) {
      case (acc, token) =>
        acc match {
          case xs :+ x =>
            if (separators.contains(token)) acc :+ token
            else if (separators.contains(x)) acc :+ token
            else xs :+ (x + token)
          case Nil =>
            List(token)
        }
    }
  }

  val left = eval("3 + 4 * 2 / ( 1 - 5 ) ^ 2 ^ 3")
  val right = Right(3 + 4 * 2 / math.pow(1 - 5, math.pow(2, 3)))
  println(s"left = $left")
  println(s"right = $right")
  assert(left == right)
  println("OK!")
}