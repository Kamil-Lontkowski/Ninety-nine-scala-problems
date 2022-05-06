def duplicateN[A](n: Int, xs: List[A]): List[A] =
  xs.flatMap(e => List.fill(n)(e))

duplicateN(3, List('a', 'b', 'c', 'c', 'd'))