package ru.tversu

import ru.tversu.util.{EllipticCurve, Point}

import scala.annotation.tailrec

object DigitalSignatureChecker {

  // переменная, хранящая данные об эллиптической кривой
  val ellipticCurve = new EllipticCurve(751, -1, 1)

  // основная функция программы для подсчёта v, u1, u2, X
  def main(sysArgs: Array[String]): Unit = {
    val rs = new Point(13, 11, 12)
    val h = 5
    val Q = new Point(13, 596, 433)
    val generatingPoint = new Point(13, 562, 89)
    
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
