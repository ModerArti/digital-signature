package ru.tversu.signature

import ru.tversu.util.{EllipticCurve, Point}

object DigitalSignatureGenerator extends SignatureTool {

  // переменная, хранящая данные об эллиптической кривой
  val ellipticCurve = new EllipticCurve(751, -1, 1)

  // основная функция программы для подсчёта k*G, r, z, s
  def generate(h: Int, d: Int, k: Int, G: Point): Unit = {
    println(s"h = $h, d = $d, k = $k, G = $G")

    val kG = calculatekG(k, G)
    println(s"k * G = $kG")

    val r = module(kG.x, G.q)
    println(s"r = $r")

    val z = module(reverseNumber(k, G.q), G.q)
    println(s"z = $z")

    val s = module(z * (h + d * r), G.q)
    println(s"s = $s")

    println(s"(r, s) = ${new Point(G.q, r, s)}")
  }

}