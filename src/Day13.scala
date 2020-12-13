package org.elmarweber.adventofcode2020

import scala.io.Source

object Day13 extends App {
  val TEST_DATA = """939
                    |7,13,x,x,59,x,31,19""".stripMargin

  def parseInput(input: String) = {
    val Array(a, b) = input.split("\n")
    (a.toInt, b.split(",").filter(_ != "x").map(_.toInt).toList)
  }

  def calculate(time: Int, buses: List[Int]): (Int, Option[Int]) = {
    buses.find(time % _ == 0) match {
      case Some(bus) => (time, Some(bus))
      case None => calculate(time + 1, buses)
    }
  }

  assert(calculate(parseInput(TEST_DATA)._1, parseInput(TEST_DATA)._2) == (944, Some(59)))
  val (start, buses) = parseInput(Source.fromFile("./adventofcode-data/day13_input.txt").mkString)
  val (end, Some(bus)) = calculate(start, buses)
  println(end, bus, (end - start) * bus)
}
