import scala.annotation.tailrec
//Put elements and consecutive repeating element inside list
// (a, a, a, b, c, c, a, a) => ((a, a, a, a), (b), (c, c), (a, a))

def pack[A](xs: List[A]): List[List[A]] =
  @tailrec
  def iter(result: List[List[A]], curr: List[A]): List[List[A]] = curr match
    case head :: _ => iter(curr.takeWhile(_ == head) :: result, curr.dropWhile(_ == head))
    case Nil => result.reverse

  iter(Nil, xs)

pack(List('a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e'))

// From site; span which
def pack1[A](ls: List[A]): List[List[A]] = {
  if (ls.isEmpty) List(List())
  else {
    val (packed, next) = ls.span(_ == ls.head)
    if (next == Nil) List(packed)
    else packed :: pack(next)
  }
}

pack1(List('a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e'))