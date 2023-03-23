import util.Util.toGrayScale

import scala.::
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

split(' ')(c).flatMap(split(',')).map(_.foldRight(Nil: List[Char])((c, acc) => c :: acc).mkString)
  .grouped(3).toList

val m = List(List(1, 1, 1, 2, 5, 3), List(1, 3, 4, 2, 5, 1), List(1, 4, 2, 1, 6, 4), List(1, 2,
  4, 5, 6, 1), List(1, 3, 2, 1, 2, 4), List(1, 3, 5, 1, 4, 4))
val k = List(List(1, 0, -1), List(1, 0, -1), List(1, 0, -1))

m.zip(k).map(x => x._1.zip(x._2))
  .foldRight(0)((x, acc) => x.foldRight(0)((y, acc) => y._1 * y._2 + acc) + acc)

k.zip(k).map(k=>k._1.zip(k._2)).map(p => p.foldRight(Nil:List[Int])((x,acc) => x._1 + x._2 :: acc))