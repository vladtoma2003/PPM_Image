//val l = List('1',',','2',',','3')

def split(d: Char)(s: List[Char]): List[List[Char]] = {
  def op(c: Char, acc: List[List[Char]]): List[List[Char]] = {
    acc match {
      case Nil => if (c == d) Nil else List(List(c))
      case x :: xs => if (c == d) Nil :: acc else (c :: x) :: xs
    }
  }

  s.foldRight(Nil: List[List[Char]])(op)
}

val l = List(1, 2, 3, 4, 5, 6, 7, 8, 9)


val c = List('1', ',', '1', ' ', '2', '5', '5', ' ', '0', ' ', '0', ' ', '0', ' ')

l.grouped(3).toList

split(' ')(c).toList.flatten.grouped(3).toList

split(' ')(c).flatMap(split(',')).flatten.grouped(3).toList

split(' ')(c).flatMap(split(',')).map(_.foldRight(Nil:List[Char])((c, acc) => c :: acc).mkString)
  .grouped(3).toList
