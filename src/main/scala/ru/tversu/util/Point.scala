package ru.tversu.util

import ru.tversu.DigitalSignatureGenerator.{ellipticCurve, module, reverseNumber}

// класс, абстрагирующий в себе точку (её координаты и порядок)
class Point(val q: Int, val x: Int, val y: Int) {

  // оператор суммирования двух точек
  def +(other: Point): Point = {
    if (other == new Point(13, 0, 0)) this
    else if (this.x == other.x) {
      val lambda = module(calculateLambda(this.x, this.y, ellipticCurve.a), ellipticCurve.p)

      val newX = module(math.pow(lambda, 2).toInt - 2 * this.x, ellipticCurve.p)
      val newY = module(lambda * (this.x - newX) - this.y, ellipticCurve.p)

      new Point(this.q, newX, newY)
    } else {
      val lambda = module(calculateLambda(this.x, this.y, other.x, other.y), ellipticCurve.p)

      val newX = module(math.pow(lambda, 2).toInt - this.x - other.x, ellipticCurve.p)
      val newY = module(lambda * (this.x - newX) - this.y, ellipticCurve.p)

      new Point(this.q, newX, newY)
    }
  }

  // метод для подсчёта лямбды при сложении двух одинаковых точек
  def calculateLambda(x1: Int, y1: Int, a: Int): Int = {
    val factor = if (y1 < 0) -1 else 1

    val numerator = factor * (3 * math.pow(x1, 2).toInt + a)
    val denominator = factor * 2 * y1

    module(numerator * reverseNumber(denominator, ellipticCurve.p), ellipticCurve.p)
  }

  // метод для подсчёта лямбды при сложении двух разных точек
  def calculateLambda(x1: Int, y1: Int, x2: Int, y2: Int): Int = {
    val factor = if (x2 - x1 < 0) -1 else 1

    val numerator = factor * (y2 - y1)
    val denominator = factor * (x2 - x1)

    module(numerator * reverseNumber(denominator, ellipticCurve.p), ellipticCurve.p)
  }

  // метод для преобразования точки в строку
  override def toString = s"Point($x, $y)"

  def canEqual(other: Any): Boolean = other.isInstanceOf[Point]

  // метод сравнения двух точек
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
