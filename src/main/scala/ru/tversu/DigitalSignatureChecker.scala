package ru.tversu

import ru.tversu.util.{EllipticCurve, Point}

object DigitalSignatureChecker extends SignatureTool {

  // переменная, хранящая данные об эллиптической кривой
  val ellipticCurve = new EllipticCurve(751, -1, 1)

  // основная функция программы для подсчёта v, u1, u2, X
  def main(sysArgs: Array[String]): Unit = {
    val rs = new Point(13, 11, 12)
    val h = 5
    val Q = new Point(13, 596, 433)
    val generatingPoint = new Point(13, 562, 89)

    if (1 <= rs.x && rs.x <= generatingPoint.q - 1 &&
      1 <= rs.y && rs.y <= generatingPoint.q - 1) {
      val v = module(reverseNumber(rs.y, generatingPoint.q), generatingPoint.q)
      println(s"v = $v")

      val u1 = module(v * h, generatingPoint.q)
      println(s"u1 = $u1")

      val u2 = module(v * rs.x, generatingPoint.q)
      println(s"u2 = $u2")

      val X1 = calculatekG(u1, generatingPoint)
      println(s"X1 = $X1")
      val X2 = calculatekG(u2, Q)
      println(s"X2 = $X2")
      val X = X1 + X2
      println(s"X = $X")

      if (rs.x == module(X.x, generatingPoint.q)) println("Signature is original")
      else println("Signature is fake")
    } else {
      println("Signature is fake")
    }
  }

}
