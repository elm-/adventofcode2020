package org.elmarweber.adventofcode2020

import scala.io.Source
import scala.util.parsing.combinator._


object Day10Step2 extends App with RegexParsers {
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

  def parseInput(input: String): List[Int] = input.split("\n").map(_.toInt).toList
  def fullInput(input: List[Int]) = input

  def findSolutions(input: List[Int]) = {
    val cache = (0 to input.max).toList.map(_ => 0L) // TODO check, better ways for memoization in scala probably
    input.foldLeft(cache.updated(0, 1L)) { case (cache, nextElem) =>
      cache.updated(nextElem,
        cache.lift(nextElem - 1).getOrElse(0L) +
        cache.lift(nextElem - 2).getOrElse(0L) +
        cache.lift(nextElem - 3).getOrElse(0L)
      )
    }
  }

  assert(findSolutions(fullInput(parseInput(TEST_DATA).sorted)).last == 19208)
  println(findSolutions(parseInput(Source.fromFile("./adventofcode-data/day10_input.txt").mkString).sorted).last)
}
