import scala.annotation.tailrec
//Split a list into two parts.
//The length of the first part is given. Use a Tuple for your result.

def split[A](n: Int, xs: List[A]): (List[A], List[A]) = (n, xs) match
  case (_, Nil) => (Nil, Nil)
  case (0, list) => (Nil, list)
  case (n, head :: tail) =>
    val (pre, post) = split(n - 1, tail)
    (head :: pre, post)

split(3, List('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k'))

// Using builtins

def splitbuilt[A](n: Int, xs: List[A]): (List[A], List[A]) =
  (xs.take(n), xs.drop(n))

splitbuilt(3, List('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k'))

def splittail[A](n: Int, xs: List[A]): (List[A], List[A]) =
  @tailrec
  def iter(count: Int, resultPre: List[A], list: List[A]): (List[A], List[A]) = (count, list) match
    case (_ , Nil) => (resultPre.reverse, list)
    case (0, l) => (resultPre.reverse, l)
    case (n, head :: tail) => iter(count-1, head :: resultPre, tail)

  iter(n, Nil, xs)

splittail(3, List('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k'))

