// Get the n-th element of list
import scala.annotation.tailrec

@tailrec
def nth[A](n: Int, xs: List[A]): A =
  (n, xs) match
    case (0, h :: _) => h
    case (n, _ :: tail) => nth(n-1, tail)
    case (_, Nil) => throw new NoSuchElementException

nth(2, List(1, 1, 2, 3, 5, 8))