@main
def main(): Unit = {
  import scala.io.StdIn._
  val name = readLine("Enter your name: ")
  println("Enter your age: ")
  val age = readInt()
  println(Console.BOLD)
  print("Name: ")
  print(Console.UNDERLINED)
  print(name)
  println(Console.BOLD)
  print("Age: ")
  print(Console.RESET)
  print(age)
}