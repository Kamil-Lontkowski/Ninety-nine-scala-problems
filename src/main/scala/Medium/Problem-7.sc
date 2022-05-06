// Flatten list
import scala.annotation.tailrec
// Using flatmap
def flatten(xs: List[Any]): List[Any] = xs flatMap {
  case ms: List[_] => flatten(ms)
  case e => List(e)
}

flatten(List(List(1, 1), 2, List(3, List(5, 8))))

// My manual approach
def flat(xs: List[Any]): List[Any] =
  @tailrec
  def iter(result: List[Any], l: List[Any]): List[Any] = l match
    case head :: tail => head match
      case a: List[_] => iter(result, a ::: tail)
      case a => iter(a :: result , tail) // We can also append iter(result :+ a, tail)
    case Nil => result                   // so there is no need to reverse
                                         // But reversing once is better then O(n) append each time
  iter(List(), xs).reverse

flat(List(List(1, 1), 2, List(3, List(5, 8))))

