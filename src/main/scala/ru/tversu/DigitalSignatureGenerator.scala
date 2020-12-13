package ru.tversu

import ru.tversu.util.{EllipticCurve, Point}

object DigitalSignatureGenerator extends SignatureTool {

  // переменная, хранящая данные об эллиптической кривой
  val ellipticCurve = new EllipticCurve(751, -1, 1)

  // основная функция программы для подсчёта k*G, r, z, s
  def main(sysArgs: Array[String]): Unit = {
    val h = 10
    val d = 5
    val k = 11
    val generatingPoint = new Point(13, 416, 55)

    val kG = calculatekG(k, generatingPoint)
    println(s"k * G = $kG")

    val r = module(kG.x, generatingPoint.q)
    println(s"r = $r")

    val z = module(reverseNumber(k, generatingPoint.q), generatingPoint.q)
    println(s"z = $z")

    val s = module(z * (h + d * r), generatingPoint.q)
    println(s"s = $s")

    println(s"(r, s) = ${new Point(generatingPoint.q, r, s)}")
  }

}