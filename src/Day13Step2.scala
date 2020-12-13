package org.elmarweber.adventofcode2020

import scala.io.Source

object Day13Step2 extends App {
  val TEST_DATA = """939
                    |7,13,x,x,59,x,31,19""".stripMargin

  def parseInput(input: String) = {
    val Array(_, b) = input.split("\n")
    b.split(",").zipWithIndex.filter(_._1 != "x").toList.map { case (a, b) => (a.toInt, b) }
  }

  def matchesCondition(nr: Long, buses: List[(Int, Int)]): Boolean = {
    buses.forall { case (busNr, offset) =>
      (nr + offset) % busNr == 0
    }
  }

  def calculate(buses: List[(Int, Int)]): Long = {
    val prioBuses = buses.sortBy(_._1).reverse
    val (ts, _) = prioBuses.foldLeft((0L, 1L)) { case ((ts, step), (busId, offset)) =>
      var curTs = ts
      while ((curTs + offset) % busId != 0) {
        curTs += step
      }
      (curTs, step * busId)
    }
    assert(matchesCondition(ts, buses)) // final sanity check
    ts
  }

  assert(calculate(parseInput(TEST_DATA)) == 1068781)
  println(calculate(parseInput(Source.fromFile("./adventofcode-data/day13_input.txt").mkString)))
}
