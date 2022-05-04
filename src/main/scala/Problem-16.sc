import scala.annotation.tailrec
// Drop every n-th element from list
def drop[A](n: Int, xs: List[A]): List[A] =
  @tailrec
  def iter(result: List[A], count: Int, list: List[A]): List[A] = list match
    case _ :: tail if count % n == 0 => iter(result, count + 1, tail)
    case head :: tail => iter(head :: result, count + 1, tail)
    case Nil => result.reverse

  iter(Nil, 1, xs)

drop(3, List('a', 'b', 'c', 'd', 'e'))

def functional[A](n: Int, xs: List[A]) =
  xs.zipWithIndex.filterNot(x => (x._2 + 1) % n == 0).map(_._1)

functional(3, List('a', 'b', 'c', 'd', 'e'))