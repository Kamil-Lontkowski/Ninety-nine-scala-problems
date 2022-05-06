// Get the length of a list
import scala.annotation.tailrec

def length[A](xs: List[A]): Int =
  @tailrec
  def iter(count: Int, tail: List[A]): Int =
    (count, tail) match
      case (c, Nil) => c
      case (c, _ :: t) => iter(c+1, t)

  iter(0, xs)


length(List(1, 1, 2, 3, 5, 8))