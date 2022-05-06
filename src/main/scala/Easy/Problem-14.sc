def duplicate[A](xs: List[A]): List[A] = xs match
  case Nil => Nil
  case head :: tail => head :: head :: duplicate(tail)

duplicate(List('a', 'b', 'c', 'c', 'd'))

//Solution from site
def duplicate1[A](ls: List[A]): List[A] =
  ls.flatMap(e => List(e, e))