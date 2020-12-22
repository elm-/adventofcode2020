package org.elmarweber.adventofcode2020

import org.apache.commons.lang3.StringUtils
import org.apache.commons.lang3.math.NumberUtils

import scala.io.Source

object Day14 extends App {
  val TEST_DATA = """mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X
                    |mem[8] = 11
                    |mem[7] = 101
                    |mem[8] = 0
                    |""".stripMargin

  sealed trait Instruction
  case class Mask(mask: String) extends Instruction
  case class Memory(address: Int, n: Long) extends Instruction

  // doing a non bit operator implementation, otherwise do masking with OR and AND bitwise to achieve the same
  def parseInput(input: String): List[Instruction] = {
    input.split("\n").toList.map(_.split(" = ")).map { case Array(left, right) =>
      if (left == "mask") {
        Mask(right)
      } else {
        val addr = left.replace("mem", "").replace("[", "").replace("]", "").toInt
        Memory(addr, right.toLong)
      }
    }
  }


  def toBinary(n: Long): String = {
    n.toBinaryString.reverse.padTo(36, "0").reverse.mkString
  }

  def applyMask(bits: String, mask: String): String = {
    bits.zip(mask).map {
      case (bit, 'X') => bit
      case (_, v) => v
    }.mkString
  }

  def calculate(instructions: List[Instruction]): Long = {
    val (mem, _) = instructions.foldLeft((Map.empty[Int, String], None: Option[String])) {
      case ((memory, _), Mask(newMask)) => (memory, Some(newMask))
      case ((memory, Some(curMask)), Memory(addr, nr)) => (memory + (addr -> applyMask(toBinary(nr), curMask)), Some(curMask))
    }
    mem.values.map(java.lang.Long.parseUnsignedLong(_, 2)).sum
  }

  assert(calculate(parseInput(TEST_DATA)) == 165)
  println(calculate(parseInput(Source.fromFile("./adventofcode-data/day14_input.txt").mkString)))
}
