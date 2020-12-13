package org.elmarweber.adventofcode2020

import scala.io.Source

object Day12Step2 extends App {
  val TEST_DATA = """F10
                    |N3
                    |F7
                    |R90
                    |F11""".stripMargin

  type Instruction = (String, Int)


  def parseInput(input: String): List[Instruction] = input.split("\n").toList.map(_.splitAt(1)).map { case (a, b) => (a, b.toInt) }

  def transposeWaypoint(wx: Int, wy: Int, o: Int): (Int, Int) = {
    if (o == 0) {
      (wx, wy)
    } else if (o > 0) {
      transposeWaypoint(wy, -wx, o - 1)
    } else if (o < 0) {
      transposeWaypoint(-wy, wx, o + 1)
    } else {
      throw new IllegalStateException("Can't happen")
    }
  }

  // TODO: cleaner way, transform N, E, S, W instructions and use same logic in calcForward and other one, make x, y tuple
  def calculate(instructions: List[Instruction], sx: Int = 0, sy: Int = 0, wx: Int = 10, wy: Int = 1): (Int, Int) = {
    println("D", sx, sy, wx, wy)
    instructions match {
      case Nil => (sx, sy)
      case ("N", v) :: tail => calculate(tail, sx, sy, wx, wy + v)
      case ("E", v) :: tail => calculate(tail, sx, sy, wx + v, wy)
      case ("S", v) :: tail => calculate(tail, sx, sy, wx, wy - v)
      case ("W", v) :: tail => calculate(tail, sx, sy, wx - v, wy)
      case ("F", v) :: tail => calculate(tail, sx + wx * v, sy + wy * v, wx, wy)
      case ("R", v) :: tail =>
        val (nwx, nwy) = transposeWaypoint(wx, wy, v / 90)
        calculate(tail, sx, sy, nwx, nwy)
      case ("L", v) :: tail =>
        val (nwx, nwy) = transposeWaypoint(wx, wy, -v / 90)
        calculate(tail, sx, sy, nwx, nwy)
    }
  }

  assert(calculate(parseInput(TEST_DATA)) == (214, -72))
  println(calculate(parseInput(Source.fromFile("./adventofcode-data/day12_input.txt").mkString)))
}
