package org.elmarweber.adventofcode2020

import scala.io.Source
import scala.util.parsing.combinator._


object Day8 extends App with RegexParsers {
  val TEST_DATA =
    """nop +0
      |acc +1
      |jmp +4
      |acc +3
      |jmp -3
      |acc -99
      |acc +1
      |jmp -4
      |acc +6""".stripMargin

  type Instruction = (String, Int, Boolean)

  def parseInstructions(input: String): List[Instruction] = {
    input.split("\n").toList.map(_.split(" ")).map {
      case Array(i, o) => (i, o.toInt, false)
    }
  }

  def exec(instructions: List[Instruction], value: Int = 0, index: Int = 0): Int = {
    instructions(index) match {
      case (_, _, true) => value
      case ("nop", o, false) => exec(instructions.updated(index, ("nop", o, true)), value, index + 1)
      case ("acc", o, false) => exec(instructions.updated(index, ("acc", o, true)), value + o, index + 1)
      case ("jmp", o, false) => exec(instructions.updated(index, ("jmp", o, true)), value, index + o)
    }
  }

  assert(exec(parseInstructions(TEST_DATA)) == 5)
  println(exec(parseInstructions(Source.fromFile("./adventofcode-data/day8_input.txt").mkString)))

}
