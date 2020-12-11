package org.elmarweber.adventofcode2020

import scala.io.Source

object Day11Step2 extends App {
  val TEST_DATA = """L.LL.LL.LL
                    |LLLLLLL.LL
                    |L.L.L..L..
                    |LLLL.LL.LL
                    |L.LL.LL.LL
                    |L.LLLLL.LL
                    |..L.L.....
                    |LLLLLLLLLL
                    |L.LLLLLL.L
                    |L.LLLLL.LL""".stripMargin

  type Field = List[List[Char]]

  def parseInput(input: String): Field = input.split("\n").toList.map(_.toList)
  def findNextSeat(input: Field, i: Int, j: Int, deltaI: Int, deltaJ: Int): Char = {
    input.lift(i + deltaI).flatMap(_.lift(j + deltaJ)) match {
      case None => '.'
      case Some('.') => findNextSeat(input, i + deltaI, j + deltaJ, deltaI, deltaJ)
      case Some(v) => v
    }
  }
  def calculateNext(input: Field): Field = {
    val output = Array.ofDim[Char](input.size, input(0).size)
    input.indices.foreach { i =>
      input(i).indices.foreach { j =>
        val seatContext = "" + findNextSeat(input, i, j, - 1, - 1) + findNextSeat(input, i, j, -1, 0) + findNextSeat(input, i, j, -1,  + 1) +
          findNextSeat(input, i, j, 0, - 1) + findNextSeat(input, i, j, 0,1) +
          findNextSeat(input, i, j, 1, - 1) + findNextSeat(input, i, j, 1, 0) + findNextSeat(input, i, j, 1, 1)
        val occupied = seatContext.count(_ == '#')
        output(i)(j) = {
          if (input(i)(j) == 'L' && occupied == 0) '#'
          else if (input(i)(j) == '#' && occupied >= 5) 'L'
          else input(i)(j)
        }
      }
    }
    output.map(_.toList).toList
  }

  def stabilize(input: Field): Int = {
    val next = calculateNext(input)
    if (next == input) input.flatten.count(_ == '#') else stabilize(next)
  }

  def printField(input: Field) = {
    input.foreach { row =>
      println(row.mkString(""))
    }
  }

  assert(stabilize(parseInput(TEST_DATA)) == 26)
  println(stabilize(parseInput(Source.fromFile("./adventofcode-data/day11_input.txt").mkString)))
}
