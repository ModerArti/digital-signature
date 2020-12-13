package ru.tversu

import ru.tversu.signature.{DigitalSignatureChecker, DigitalSignatureGenerator}
import ru.tversu.util.Point

import scala.io.StdIn

object Menu {

  def main(args: Array[String]): Unit = {
    println(
      """Select the command:
        |1) Generate signature - insert '1';
        |2) Check signature - insert '2'.
        |""".stripMargin)

    val command = StdIn.readInt()

    command match {
      case 1 =>
        println("Enter parameters:")
        print("h = ")
        val h = StdIn.readInt()
        print("d = ")
        val d = StdIn.readInt()
        print("k = ")
        val k = StdIn.readInt()
        print("q = ")
        val q = StdIn.readInt()
        print("G = ")
        val G = StdIn.readLine().split(" ").map(_.toInt)
        println("Results:")
        DigitalSignatureGenerator.generate(h, d, k,
          new Point(q, G(0), G(1))
        );
      case 2 =>
        println("Enter parameters:")
        print("rs = ")
        val rs = StdIn.readLine().split(" ").map(_.toInt)
        print("h = ")
        val h = StdIn.readInt()
        print("Q = ")
        val Q = StdIn.readLine().split(" ").map(_.toInt)
        print("G = ")
        val G = StdIn.readLine().split(" ").map(_.toInt)
        print("q = ")
        val q = StdIn.readInt()
        println("Results:")
        DigitalSignatureChecker.check(
          new Point(q, rs(0), rs(1)),
          h,
          new Point(q, Q(0), Q(1)),
          new Point(q, G(0), G(1)),
        )
      case _ =>
        println("Wrong command. Choose the correct one")
    }
  }

}
