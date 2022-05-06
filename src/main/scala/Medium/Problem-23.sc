import scala.annotation.tailrec
import util.Random

@tailrec
def randomSelect[A](n: Int, xs: List[A]): List[A] =
  if xs.length == n then xs
  else
    val (pre, post) = xs.splitAt(Random.nextInt(xs.length))
    randomSelect(n, pre ++ post.tail)

randomSelect(3, List('a', 'b', 'c', 'd', 'f', 'g', 'h'))
