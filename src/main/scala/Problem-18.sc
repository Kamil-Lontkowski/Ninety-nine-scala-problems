import scala.annotation.tailrec
// Get slice from list I inclusive K exclusive

def slice[A](I:Int, K: Int, xs: List[A]): List[A] = (I, K, xs) match
  case (_ , 0, _) => Nil
  case (_, _, Nil) => Nil
  case (0, k, head :: tail) => head :: slice(0, k-1, tail)
  case (i, k, _ :: tail) => slice(i-1, k-1, tail)

slice(3, 7, List('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k'))

// Using builtins

def sliceF[A](I:Int, K: Int, xs: List[A]): List[A] =
  xs.slice(I, I + K - I.max(0)) //xs.drop(I).take(K-(I.max(0)))

sliceF(3, 7, List('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k'))

// tailrec from site
def sliceTailRecursive[A](start: Int, end: Int, ls: List[A]): List[A] =
  @tailrec
  def sliceR(count: Int, curList: List[A], result: List[A]): List[A] =
    (count, curList) match
      case (_, Nil)                     => result.reverse
      case (c, _ :: _) if end <= c   => result.reverse
      case (c, h :: tail) if start <= c => sliceR(c + 1, tail, h :: result)
      case (c, _ :: tail)               => sliceR(c + 1, tail, result)

  sliceR(0, ls, Nil)

sliceTailRecursive(3, 7, List('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k'))
