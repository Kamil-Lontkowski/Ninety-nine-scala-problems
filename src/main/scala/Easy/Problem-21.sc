import scala.annotation.tailrec
def insertAt[A](elem: A, n: Int, xs: List[A]): List[A] = (n, xs) match
  case (_, Nil) => throw new IndexOutOfBoundsException
  case (0, head :: tail) => elem :: head :: tail
  case (c, head :: tail) => head :: insertAt(elem, c-1, tail)

def insertAtTail[A](elem: A, n: Int, xs: List[A]): List[A] =
  @tailrec
  def iter(n: Int, result: List[A], list: List[A]): List[A] = (n, list) match
    case (_, Nil) => throw new IndexOutOfBoundsException
    case (0, l) => result.reverse ::: elem :: l
    case (c, head :: tail) => iter(c-1, head::result, tail)

  iter(n, Nil, xs)

def insertAtBuiltins[A](elem: A, n: Int, xs: List[A]): List[A] = xs.splitAt(n) match {
  case (pre, post) => pre ::: elem :: post
}

insertAt('n', 2, List('a', 'b', 'c', 'd'))
insertAtTail('n', 2, List('a', 'b', 'c', 'd'))
insertAtBuiltins('n', 2, List('a', 'b', 'c', 'd'))