package org.elmarweber.adventofcode2020

import scala.io.Source
import scala.util.parsing.combinator._


object Day10 extends App with RegexParsers {
  val TEST_DATA =
    """28
      |33
      |18
      |42
      |31
      |14
      |46
      |20
      |48
      |47
      |24
      |23
      |49
      |45
      |19
      |38
      |39
      |11
      |1
      |32
      |25
      |35
      |8
      |17
      |7
      |9
      |4
      |2
      |34
      |10
      |3""".stripMargin

  def parseInput(input: String): List[Int] = 0 :: input.split("\n").map(_.toInt).toList

  def calculate(input: List[Int]): (Int, Int) = {
    val size = input
      .sorted
      .sliding(2).toList
      .map { case List(a, b) => b - a }
      .groupBy(a => a)
      .mapValues(_.size)
    (size(1), size(3) + 1)
  }

  assert(calculate(parseInput(TEST_DATA)) == (22, 10))
  println(calculate(parseInput(Source.fromFile("./adventofcode-data/day10_input.txt").mkString)))
}
