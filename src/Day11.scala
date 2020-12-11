package org.elmarweber.adventofcode2020

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Random

object Day11 extends App {
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
  def getSafe(input: Field, i: Int, j: Int): Char = {
    input.lift(i).flatMap(_.lift(j)).getOrElse('.')
  }
  def calculateNext(input: Field): Field = {
    val output = Array.ofDim[Char](input.size, input(0).size)
    input.indices.foreach { i =>
      input(i).indices.foreach { j =>
        val seatContext = "" + getSafe(input, i - 1, j - 1) + getSafe(input, i - 1, j) + getSafe(input, i - 1, j + 1) +
          getSafe(input, i, j - 1) + getSafe(input, i, j + 1) +
          getSafe(input, i + 1, j - 1) + getSafe(input, i + 1, j) + getSafe(input, i + 1, j + 1)
        val occupied = seatContext.count(_ == '#')
        output(i)(j) = {
          if (input(i)(j) == 'L' && occupied == 0) '#'
          else if (input(i)(j) == '#' && occupied >= 4) 'L'
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

  assert(stabilize(parseInput(TEST_DATA)) == 37)
  println(stabilize(parseInput(Source.fromFile("./adventofcode-data/day11_input.txt").mkString)))
}
