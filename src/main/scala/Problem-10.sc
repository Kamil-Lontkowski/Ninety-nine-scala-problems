import scala.annotation.tailrec
//Group consecutive repeating element as tuple (size, element)

def pack[A](xs: List[A]): List[List[A]] =
  @tailrec
  def iter(result: List[List[A]], curr: List[A]): List[List[A]] = curr match
    case head :: _ => iter(curr.takeWhile(_ == head) :: result, curr.dropWhile(_ == head))
    case Nil => result.reverse

  iter(Nil, xs)

def encode[A](xs: List[A]): List[(Int, A)] =
  pack(xs).map(x => (x.length, x.head))

def encodeworse[A](xs: List[A]): List[(Int, A)] =
  val packed = pack(xs)
  @tailrec
  def iter(result: List[(Int, A)], list: List[List[A]]): List[(Int, A)] = list match
    case head :: tail => iter((head.length, head.head) :: result, tail)
    case Nil => result.reverse

  iter(Nil, packed)

encode(List('a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e'))
encodeworse(List('a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e'))
