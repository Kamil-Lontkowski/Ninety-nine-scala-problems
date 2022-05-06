import util.Random

def lotto(n: Int, end: Int): List[Int] =
  val list = List.range(1, end)
  Random.shuffle(list).take(n)

lotto(6, 49)