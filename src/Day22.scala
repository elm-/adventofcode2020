package org.elmarweber.adventofcode2020

import scala.io.Source

object Day22 extends App {
  val TEST_DATA = """Player 1:
                    |9
                    |2
                    |6
                    |3
                    |1
                    |
                    |Player 2:
                    |5
                    |8
                    |4
                    |7
                    |10""".stripMargin

  def parseInput(input: String): (List[Int], List[Int]) = {
    def toIntList(input: String) = input.split("\n").toList.tail.map(_.toInt)
    val Array(p1, p2) = input.split("\n\n")
    (toIntList(p1), toIntList(p2))
  }

  def play(p1: List[Int], p2: List[Int]): (List[Int], List[Int]) = (p1.headOption, p2.headOption) match {
    case (None, _) | (_, None) => (p1, p2)
    case (Some(v1), Some(v2)) if v1 > v2 => play(p1.tail ::: List(v1, v2), p2.tail)
    case (Some(v1), Some(v2)) if v1 < v2 => play(p1.tail, p2.tail ::: List(v2, v1))
    case (Some(v1), Some(v2)) if v1 == v2 => throw new IllegalStateException(s"Would be stuck at ${p1} ${p2}")
  }

  def calculate(p1: List[Int], p2: List[Int]) = {
    val (finalP1, finalP2) = play(p1, p2)
    (finalP1 ::: finalP2).reverse.zipWithIndex.map { case (a, b) => a * (b + 1) }.sum
  }

  assert(calculate(parseInput(TEST_DATA)._1, parseInput(TEST_DATA)._2) == 306)
  val (p1, p2) = parseInput(Source.fromFile("./adventofcode-data/day22_input.txt").mkString)
  println(calculate(p1, p2))
}
