import scala.annotation.tailrec

def range(start: Int, end: Int): List[Int] =
  if start > end then Nil
  else start :: range(start+1, end)

def rangeTail(start: Int, end: Int): List[Int] = {
  @tailrec
  def iter(end: Int, result: List[Int]): List[Int] = {
    if (end < start) result
    else iter(end - 1, end :: result)
  }
  iter(end, Nil)
}

range(4, 9)
rangeTail(4, 9)