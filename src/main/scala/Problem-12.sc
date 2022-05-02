def decode[A](xs: List[(Int, A)]): List[A] =
  xs.flatMap(e => List.fill(e._1)(e._2))


decode(List((4,'a'), (1,'b'), (2,'c'), (2,'a'), (1,'d'), (4,'e')))