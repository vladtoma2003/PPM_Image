import util.Pixel

// Online viewer: https://0xc0de.fr/webppm/
object Solution {
  type Image = List[List[Pixel]]
  type GrayscaleImage = List[List[Double]]

  // prerequisites
  def fromStringPPM(image: List[Char]): Unit = {
    val noType = image.drop(3)
    val lungime = makeString(noType).toInt
    println(lungime)
    val inaltime = makeString(noType.drop(makeString(noType).length + 1)).toInt
    println(inaltime)
    val pixels = noType.drop(makeString(noType).length + makeString(noType.drop(makeString(noType)
      .length + 1)).length + 2 + 4)
    val charList = pixels.foldLeft(Nil:List[List[Char]])((acc, c) =>  {
      acc match {
        case Nil =>  if(c == ' ' || c == '\n') Nil else List(List(c))
        case x :: xs => if(c == ' ' || c == '\n') acc :: Nil else (c :: x) :: xs
      }
    })
  }

  def makeString(l: List[Char]): String = {
    if (l.head == ' ' || l.head == '\n') "" else l.head + makeString(l.tail)
  }

  def toStringPPM(image: Image): List[Char] = ???

  // ex 1
  def verticalConcat(image1: Image, image2: Image): Image = ???

  // ex 2
  def horizontalConcat(image1: Image, image2: Image): Image = ???

  // ex 3
  def rotate(image: Image, degrees: Integer): Image = ???

  // ex 4
  val gaussianBlurKernel: GrayscaleImage = List[List[Double]](
    List(1, 4, 7, 4, 1),
    List(4, 16, 26, 16, 4),
    List(7, 26, 41, 26, 7),
    List(4, 16, 26, 16, 4),
    List(1, 4, 7, 4, 1)
  ).map(_.map(_ / 273))

  val Gx: GrayscaleImage = List(
    List(-1, 0, 1),
    List(-2, 0, 2),
    List(-1, 0, 1)
  )

  val Gy: GrayscaleImage = List(
    List(1, 2, 1),
    List(0, 0, 0),
    List(-1, -2, -1)
  )

  def edgeDetection(image: Image, threshold: Double): Image = ???

  def applyConvolution(image: GrayscaleImage, kernel: GrayscaleImage): GrayscaleImage = ???

  // ex 5
  def moduloPascal(m: Integer, funct: Integer => Pixel, size: Integer): Image = ???
}
