import util.Pixel
import util.Util.{getNeighbors, toGrayScale}

// Online viewer: https://0xc0de.fr/webppm/
object Solution {
  type Image = List[List[Pixel]]
  type GrayscaleImage = List[List[Double]]

  // prerequisites
  def fromStringPPM(image: List[Char]): Image = {
    val noType = image.drop(3)
    val lungime = makeString(noType).toInt
    //    println(lungime)
    val inaltime = makeString(noType.drop(makeString(noType).length + 1)).toInt
    //    println(inaltime)
    val pixels = noType.drop(makeString(noType).length + makeString(noType.drop(makeString(noType)
      .length + 1)).length + 2 + 4)
    val groupedPixels = split('\n')(pixels).flatMap(split(' '))
      .map(_.foldRight(Nil: List[Char])((c, acc) => c :: acc).mkString).grouped(3).toList
    //    println(groupedPixels)

    val pixelList = groupedPixels.map(p => makePixel(p))
    pixelList.grouped(lungime).toList
  }

  def makePixel(l: List[String]): Pixel = {
    Pixel(l.head.toInt, l.tail.head.toInt, l.drop(2).head.toInt)
  }

  def split(d: Char)(s: List[Char]): List[List[Char]] = {
    def op(c: Char, acc: List[List[Char]]): List[List[Char]] = {
      acc match {
        case Nil => if (c == d) Nil else List(List(c))
        case x :: xs => if (c == d) Nil :: acc else (c :: x) :: xs
      }
    }

    s.foldRight(Nil: List[List[Char]])(op)
  }

  def makeString(l: List[Char]): String = {
    if (l.head == ' ' || l.head == '\n') "" else l.head + makeString(l.tail)
  }

  def toStringPPM(image: Image): List[Char] = {
    ("P3\n" + image.head.length + ' ' + image.length + '\n' + "255\n" + image.flatten
      .foldRight("")((c, acc) => (c.red.toString + ' ' + c.green.toString + ' ' + c.blue
        .toString + '\n') + acc).toString).toList
  }

  // ex 1
  def verticalConcat(image1: Image, image2: Image): Image = image1 ++ image2

  // ex 2
  def horizontalConcat(image1: Image, image2: Image): Image = image1
    .zip(image2).map { case (a, b) => a ++ b }

  // ex 3

  def rotate(image: Image, degrees: Integer): Image = {
    def transpose(i: Image): Image = { // transpunerea unei matrici data la curs
      i match {
        case Nil :: _ => Nil
        case _ => i.map(_.head) :: transpose(i.map(_.tail))
      }
    }

    if (degrees == 0) image
    else rotate(transpose(image).reverse, degrees - 90)
  }

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

  def edgeDetection(image: Image, threshold: Double): Image = {
    val gray = image.map(l => l.map(p => toGrayScale(p)))

    val blur = applyConvolution(gray, gaussianBlurKernel)

    val Mx = applyConvolution(blur, Gx)
    val My = applyConvolution(blur, Gy)

    val img = Mx.zip(My).map(k => k._1.zip(k._2))
      .map(p => p.foldRight(Nil: List[Double])((x, acc) => x._1.abs + x._2.abs :: acc))

    img.map(p => p.map(q => if(q <= threshold) Pixel(0,0,0) else Pixel(255,255,2555)))

  }

  // convolutie: fac perechi cu zip, inmultesc perechile si le adun
  //GrayScaleImage
  def applyConvolution(image: GrayscaleImage, kernel: GrayscaleImage): GrayscaleImage = {
    val mats = getNeighbors(image, (kernel.length - 1) / 2)
    mats.map(m => m.map(l => l.zip(kernel).map(x => x._1.zip(x._2)
      .map(y=>y._1*y._2).sum).sum))
//      .foldRight(0: Double)((y, acc) => y._2 * y._1 + acc))
//      .foldRight(0: Double)((z, acc) => z + acc)))
  }

  // ex 5
  def moduloPascal(m: Integer, funct: Integer => Pixel, size: Integer): Image = ???
}