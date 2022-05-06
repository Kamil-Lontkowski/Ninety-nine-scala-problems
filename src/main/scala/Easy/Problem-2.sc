// Last but one element of a list
import scala.annotation.tailrec

@tailrec
def lastbutone[A](xs: List[A]): A =
  xs match
    case a :: _ :: Nil => a
    case _ :: tail => lastbutone(tail)
    case _ => throw new NoSuchElementException


lastbutone(List(1, 1, 2, 3, 5, 8))