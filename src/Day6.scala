package org.elmarweber.adventofcode2020

import scala.io.Source

object Day6 extends App {

  val TEST_DATA =
    """|abc
      |
      |a
      |b
      |c
      |
      |ab
      |ac
      |
      |a
      |a
      |a
      |a
      |
      |b""".stripMargin

  def checksum(input: String): Int = {
    input
      .split("\n\n")
      .map(_.replace("\n", "").distinct.length)
      .sum
  }

  assert(checksum(TEST_DATA) == 11)
  println(checksum(Source.fromFile("./adventofcode-data/day6_input.txt").mkString))
}
