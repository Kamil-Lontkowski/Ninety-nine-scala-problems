import scala.annotation.tailrec

def removeAt[A](n: Int, xs: List[A]): (List[A], A) = (n, xs) match
  case (_, Nil) => throw new NoSuchElementException
  case (0, head :: tail) => (tail, head)
  case (c, head :: tail) =>
    val (t, e) = removeAt(c - 1, tail)
    (head :: t, e)

def removeAtTail[A](n: Int, xs: List[A]): (List[A], A) =
  @tailrec
  def iter[A](n: Int, result: List[A], xs: List[A]): (List[A], A) = (n, xs) match
    case (_, Nil) => throw new NoSuchElementException
    case (0, head :: tail) => (result ++ tail, head)
    case (c, head :: tail) => iter(c-1, result :+ head, tail)

  iter(n, Nil, xs)

def removeAtBuiltins[A](n: Int, xs: List[A]): (List[A], A) =
  val (removed, rest) = xs.zipWithIndex.partition(_._2 == n)
  (rest.map(_._1), removed.head._1)

removeAt(1, List('a', 'b', 'c', 'd'))
removeAtTail(1, List('a', 'b', 'c', 'd'))
removeAtBuiltins(1, List('a', 'b', 'c', 'd'))