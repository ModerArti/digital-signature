package ru.tversu

import ru.tversu.util.Point

import scala.annotation.tailrec

class SignatureTool {

  // рекурсивная функция, считающая модуль числа
  @tailrec
  final def module(number: Int, mod: Int): Int = {
    if (number > 0) number % mod
    else module(number + mod, mod)
  }

  // рекурсивная функция, которая ищет число, обратное данному (по модулю)
  @tailrec
  final def reverseNumber(number: Int, mod: Int, k: Int = 1): Int = {
    if (module(number * k, mod) == 1) k
    else reverseNumber(number, mod, k + 1)
  }

  // рекурсивная функция, считающая произведение k * G
  @tailrec
  final def calculatekG(k: Int, G: Point, acc: Point = new Point(13, 0, 0)): Point = {
    if (k == 0) acc
    else calculatekG(k - 1, G, G + acc)
  }

}
