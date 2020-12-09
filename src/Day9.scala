package org.elmarweber.adventofcode2020

import org.elmarweber.adventofcode2020.Day1.Result

import scala.io.Source
import scala.util.parsing.combinator._


object Day9 extends App with RegexParsers {
  val TEST_DATA =
    """35
      |20
      |15
      |25
      |47
      |40
      |62
      |55
      |65
      |95
      |102
      |117
      |150
      |182
      |127
      |219
      |299
      |277
      |309
      |576""".stripMargin

  def parseInput(input: String): List[Long] = input.split("\n").map(_.toLong).toList

  //day 1
  def efficientSolution(input: List[Long], sum: Long): Boolean = {
    for (i <- input.indices) {
      for (j <- (i + 1) until input.size) {
        if (input(i) + input(j) == sum) return true
      }
    }
    false
  }

  def findWrongNumber(input: List[Long], window: Int = 25): Option[Long] = {
    input
      .sliding(window + 1, 1).find { numbers =>
        numbers.splitAt(window) match {
          case (preamble, List(checksum)) =>
            !efficientSolution(preamble, checksum)
        }
      }
      .map(_.last)
  }

  assert(findWrongNumber(parseInput(TEST_DATA), window = 5) == Some(127))
  println(findWrongNumber(parseInput(Source.fromFile("./adventofcode-data/day9_input.txt").mkString)))

}
