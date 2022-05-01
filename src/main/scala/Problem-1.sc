// Last element of list
import scala.annotation.tailrec

@tailrec
def last[A](xs: List[A]): A =
  xs match
    case h :: Nil => h
    case _ :: t => last(t)
    case _ => throw new NoSuchElementException


last(List(1, 1, 2, 3, 5, 8))