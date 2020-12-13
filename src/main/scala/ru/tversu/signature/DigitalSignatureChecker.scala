package ru.tversu.signature

import ru.tversu.util.{EllipticCurve, Point}

object DigitalSignatureChecker extends SignatureTool {

  // переменная, хранящая данные об эллиптической кривой
  val ellipticCurve = new EllipticCurve(751, -1, 1)

  // основная функция программы для подсчёта v, u1, u2, X
  def check(rs: Point, h: Int, Q: Point, G: Point): Unit = {
    if (1 <= rs.x && rs.x <= G.q - 1 &&
      1 <= rs.y && rs.y <= G.q - 1) {
      val v = module(reverseNumber(rs.y, G.q), G.q)
      println(s"v = $v")

      val u1 = module(v * h, G.q)
      println(s"u1 = $u1")

      val u2 = module(v * rs.x, G.q)
      println(s"u2 = $u2")

      val X1 = calculatekG(u1, G)
      println(s"X1 = $X1")
      val X2 = calculatekG(u2, Q)
      println(s"X2 = $X2")
      val X = X1 + X2
      println(s"X = $X")

      if (rs.x == module(X.x, G.q)) println("Signature is original")
      else println("Signature is fake")
    } else {
      println("Signature is fake")
    }
  }

}
