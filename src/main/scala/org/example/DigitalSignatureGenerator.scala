package org.example

import scala.annotation.tailrec

object DigitalSignatureGenerator {

  val ellipticCurve = new EllipticCurve(751, -1, 1)

  def main(sysArgs: Array[String]): Unit = {
    val h = 10
    val d = 5
    val k = 11
    val generatingPoint = new Point(13, 416, 55)

    val kG = calculatekG(k, generatingPoint)
    print(kG)
  }

  @tailrec
  def module(number: Int): Int = {
    if (number > 0) number % ellipticCurve.p
    else module(number + ellipticCurve.p)
  }

  @tailrec
  def reverseNumber(number: Int, k: Int = 1): Int = {
    if (module(number * k) == 1) k
    else reverseNumber(number, k + 1)
  }

  @tailrec
  def calculatekG(k: Int, G: Point, acc: Point = new Point(13, 0, 0)): Point = {
    if (k == 0) acc
    else calculatekG(k - 1, G, G + acc)
  }

  class Point(val q: Int, val x: Int, val y: Int) {

    def +(other: Point): Point = {
      if (other == new Point(13, 0, 0)) this
      else if (this == other) {
        val lambda = module(calculateLambda(this.x, this.y, ellipticCurve.a))

        val newX = module(math.pow(lambda, 2).toInt - 2 * this.x)
        val newY = module(lambda * (this.x - newX) - this.y)

        new Point(this.q, newX, newY)
      } else {
        val lambda = module(calculateLambda(this.x, this.y, other.x, other.y))

        val newX = module(math.pow(lambda, 2).toInt - this.x - other.x)
        val newY = module(lambda * (this.x - newX) - this.y)

        new Point(this.q, newX, newY)
      }
    }

    def calculateLambda(x1: Int, y1: Int, a: Int): Int = {
      val factor = if (y1 < 0) -1 else 1

      val numerator = factor * (3 * math.pow(x1, 2).toInt + a)
      val denominator = factor * 2 * y1

      module(numerator * reverseNumber(denominator))
    }

    def calculateLambda(x1: Int, y1: Int, x2: Int, y2: Int): Int = {
      val factor = if (x2 - x1 < 0) -1 else 1

      val numerator = factor * (y2 - y1)
      val denominator = factor * (x2 - x1)

      module(numerator * reverseNumber(denominator))
    }

    override def toString = s"Point($x, $y)"

    def canEqual(other: Any): Boolean = other.isInstanceOf[Point]

    override def equals(other: Any): Boolean = other match {
      case that: Point =>
        (that canEqual this) &&
          x == that.x &&
          y == that.y
      case _ => false
    }

    override def hashCode(): Int = {
      val state = Seq(x, y)
      state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
    }
  }

  class EllipticCurve(val p: Int, val a: Int, val b: Int)

}