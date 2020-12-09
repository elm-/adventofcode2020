package org.elmarweber.adventofcode2020

import scala.io.Source
import scala.util.parsing.combinator._


object Day9Step2 extends App with RegexParsers {
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

  // TODO: inifficient, but only had 5min, fix later
  def findInvalidSum(input: List[Long], sum: Long): (Long, Long) = {
    for (i <- input.indices) {
      for (j <- (i + 1) until input.size) {
        val slice = input.slice(i, j)
        if (slice.sum == sum) return (slice.min, slice.max)
      }
    }
    throw new IllegalArgumentException("Nothing found")
  }

  assert(findInvalidSum(parseInput(TEST_DATA), sum = 127) == (15, 47))
  println(findInvalidSum(parseInput(Source.fromFile("./adventofcode-data/day9_input.txt").mkString), sum = 31161678))
}
5453868