package ru.tversu

import scala.annotation.tailrec
import ru.tversu.util.{EllipticCurve, Point}

object DigitalSignatureGenerator {

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

  // рекурсивная функция, считающая модуль числа
  @tailrec
  def module(number: Int, mod: Int): Int = {
    if (number > 0) number % mod
    else module(number + mod, mod)
  }

  // рекурсивная функция, которая ищет число, обратное данному (по модулю)
  @tailrec
  def reverseNumber(number: Int, mod: Int, k: Int = 1): Int = {
    if (module(number * k, mod) == 1) k
    else reverseNumber(number, mod, k + 1)
  }

  // рекурсивная функция, считающая произведение k * G
  @tailrec
  def calculatekG(k: Int, G: Point, acc: Point = new Point(13, 0, 0)): Point = {
    if (k == 0) acc
    else calculatekG(k - 1, G, G + acc)
  }

}