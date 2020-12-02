package org.elmarweber.adventofcode2020

import scala.io.Source

object Day2Part2 extends App {
  val pattern = "^([0-9]+)-([0-9]+) ([a-z]): ([a-z]+)$".r
  case class Data(min: Int, max: Int, letter: Char, password: String)

  val testData =
    """
      |1-3 a: abcde
      |1-3 b: cdefg
      |2-9 c: ccccccccc""".stripMargin

  val validCount = Source.fromFile("./adventofcode-data/day2_input.txt").getLines()
    .filter(_.nonEmpty) // check for empty lines
    .map { line =>
      val matchData = pattern.findAllIn(line).matchData.toList.head
      val data = Data(matchData.group(1).toInt, matchData.group(2).toInt, matchData.group(3)(0), matchData.group(4))
      assert(data.password.nonEmpty)
      data
    }.count { data =>
      (data.password(data.min - 1) == data.letter) != (data.password(data.max - 1) == data.letter)
    }
  println(validCount)
}
