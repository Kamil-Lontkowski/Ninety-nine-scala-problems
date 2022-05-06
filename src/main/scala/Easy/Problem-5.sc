// Reverse list
import scala.annotation.tailrec

def reverse[A](xs: List[A]): List[A] =
  xs match
    case Nil => Nil
    case h :: tail => reverse(tail) ::: List(h)

reverse(List(1, 1, 2, 3, 5, 8)) == List(8, 5, 3, 2, 1, 1)

def tailreverse[A](xs: List[A]): List[A] =
  @tailrec
  def iter(result: List[A], current: List[A]): List[A] =
    current match
      case Nil => result
      case h :: tail => iter(h :: result, tail)

  iter(List(), xs)

tailreverse(List(1, 1, 2, 3, 5, 8)) == List(8, 5, 3, 2, 1, 1)