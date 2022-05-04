import scala.annotation.tailrec
// Rotate list N places to the left

@tailrec
def rotate[A](n: Int, xs: List[A]): List[A] =
  val nBounded = if xs.isEmpty then 0 else n % xs.length
  if nBounded < 0 then rotate(nBounded + xs.length, xs)
  else xs.drop(nBounded) ++ xs.take(nBounded)

rotate(3, List('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k'))
rotate(-2, List('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k'))