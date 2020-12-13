package org.elmarweber.adventofcode2020

import scala.io.Source

object Day12 extends App {
  val TEST_DATA = """F10
                    |N3
                    |F7
                    |R90
                    |F11""".stripMargin

  type Instruction = (String, Int)


  def parseInput(input: String): List[Instruction] = input.split("\n").toList.map(_.splitAt(1)).map { case (a, b) => (a, b.toInt) }

  // 90 EAST, 180 SOUTH, 270 WEST, 0 NORTH
  def calcForward(o: Int, x: Int, y: Int, v: Int): (Int, Int) = o match {
    case 0 => (x, y + v)
    case 90 => (x + v, y)
    case 180 => (x, y - v)
    case 270 => (x - v, y)
  }

  // TODO: cleaner way, transform N, E, S, W instructions and use same logic in calcForward and other one, make x, y tuple
  def calculate(instructions: List[Instruction], x: Int = 0, y: Int = 0, o: Int = 90): (Int, Int) = instructions match {
    case Nil => (x, y)
    case ("N", v) :: tail => calculate(tail, x, y + v, o)
    case ("E", v) :: tail => calculate(tail, x + v, y, o)
    case ("S", v) :: tail => calculate(tail, x, y - v, o)
    case ("W", v) :: tail => calculate(tail, x - v, y, o)
    case ("F", v) :: tail =>
      val (nx, ny) = calcForward(o, x, y, v)
      calculate(tail, nx, ny, o)
    case ("R", v) :: tail => calculate(tail, x, y, (o + v + 360) % 360)
    case ("L", v) :: tail => calculate(tail, x, y, (o - v + 360) % 360)
  }


  assert(calculate(parseInput(TEST_DATA)) == (17, -8))
  println(calculate(parseInput(Source.fromFile("./adventofcode-data/day12_input.txt").mkString)))
}
