import scala.annotation.tailrec
// Delete consecutive duplicates, remain order

def compress[A](xs: List[A]): List[A] = xs.foldRight(List[A]()) { (el, acc) =>
  if acc.isEmpty || acc.head != el then el:: acc
  else acc
}

def compress1[A](xs: List[A]): List[A] = xs match
  case Nil => Nil
  case head :: tail => head :: compress1(tail.dropWhile(_ == head))

val compressval = [A] => (xs: List[A]) => xs match
  case Nil => Nil
  case head :: tail => head :: compress1(tail.dropWhile(_ == head))

def compresstail[A](xs: List[A]): List[A] =
  @tailrec
  def iter(result: List[A], curr: List[A]): List[A] = curr match
    case head :: tail if result.nonEmpty && result.head == head => iter(result, tail)
    case head :: tail => iter(head :: result, tail)
    case Nil => result.reverse

  iter(List[A](), xs)

// Also can be done similar to first but with tailrec

compress(List('a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e'))
compress1(List('a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e'))
compressval(List('a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e'))
compresstail(List('a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e'))