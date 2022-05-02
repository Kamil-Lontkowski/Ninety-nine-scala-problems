import scala.annotation.tailrec

def encodeDirect[A](xs: List[A]): List[(Int, A)] =
  @tailrec
  def iter(result: List[(Int, A)], list: List[A]): List[(Int, A)] = list match
    case head :: tail =>
      val (packed, next) = tail.span(_ == head)
      iter((packed.length + 1, head) :: result, next)
    case Nil => result.reverse

  iter(Nil, xs)

// Solution from site
def encodeDirect1[A](ls: List[A]): List[(Int, A)] =
  if (ls.isEmpty) Nil
  else {
    val (packed, next) = ls.span(_ == ls.head)
    (packed.length, packed.head) :: encodeDirect(next)
  }

encodeDirect(List('a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e'))