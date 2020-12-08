package org.elmarweber.adventofcode2020

import scala.io.Source
import scala.util.parsing.combinator._


object Day8Step2 extends App {
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


  def exec(instructions: List[Instruction], value: Int = 0, index: Int = 0): Option[Int] = {
    instructions.lift(index) match {
      case None => Some(value)
      case Some((_, _, true)) => None
      case Some(("nop", o, false)) => exec(instructions.updated(index, ("nop", o, true)), value, index + 1)
      case Some(("acc", o, false)) => exec(instructions.updated(index, ("acc", o, true)), value + o, index + 1)
      case Some(("jmp", o, false)) => exec(instructions.updated(index, ("jmp", o, true)), value, index + o)
    }
  }

  def generateAllVariants(instructions: List[Instruction]): List[List[Instruction]] = {
    instructions.zipWithIndex.map {
      case (("acc", o, b), index) => instructions.updated(index, ("acc", o, b))
      case (("nop", o, b), index) => instructions.updated(index, ("jmp", o, b))
      case (("jmp", o, b), index) => instructions.updated(index, ("nop", o, b))
    }
  }

  def executeAll(instructions: List[Instruction]): List[Int] = {
    generateAllVariants(instructions)
      .map(v => exec(v))
      .collect {
        case Some(r) => r
      }
  }

  assert(executeAll(parseInstructions(TEST_DATA)) == List(8))
  println(executeAll(parseInstructions(Source.fromFile("./adventofcode-data/day8_input.txt").mkString)))

}
