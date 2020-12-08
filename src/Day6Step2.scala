package org.elmarweber.adventofcode2020

import scala.io.Source

object Day6Step2 extends App {

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
      .split("\n\n").toList
      .map(_.split("\n").toList.map(_.toList))
      .map { case head :: tail =>
        tail.foldLeft(head) { case (superset, element) => superset.intersect(element) }
      }
      .map(_.size)
      .sum
  }

  assert(checksum(TEST_DATA) == 6)
  println(checksum(Source.fromFile("./adventofcode-data/day6_input.txt").mkString))
}
