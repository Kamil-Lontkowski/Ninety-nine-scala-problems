import scala.annotation.tailrec

def pack[A](xs: List[A]): List[List[A]] =
  @tailrec
  def iter(result: List[List[A]], curr: List[A]): List[List[A]] = curr match
    case head :: _ => iter(curr.takeWhile(_ == head) :: result, curr.dropWhile(_ == head))
    case Nil => result.reverse

  iter(Nil, xs)

def encode[A](xs: List[A]): List[(Int, A) | A] =
  pack(xs).map{ x =>
    if x.length == 1 then x.head
    else (x.length, x.head)
  }

encode(List('a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e'))