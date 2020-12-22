package org.elmarweber.adventofcode2020

import scala.io.Source

object Day14Step2 extends App {
  val TEST_DATA = """mask = 000000000000000000000000000000X1001X
                    |mem[42] = 100
                    |mask = 00000000000000000000000000000000X0XX
                    |mem[26] = 1""".stripMargin

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

  def fromBinary(str: String): Long = {
    java.lang.Long.parseUnsignedLong(str, 2)
  }

  def applyMask(bits: String, mask: String): String = {
    bits.zip(mask).map {
      case (bit, '0') => bit
      case (_, v) => v
    }.mkString
  }

  def generatePermutations(bits: String): List[String] = {
    if (bits.contains("X")) {
      generatePermutations(bits.replaceFirst("X", "0")) :::
        generatePermutations(bits.replaceFirst("X", "1"))
    } else {
      List(bits)
    }
  }

  def calculate(instructions: List[Instruction]): Long = {
    val (mem, _) = instructions.foldLeft((Map.empty[Long, Long], None: Option[String])) {
      case ((memory, _), Mask(newMask)) => (memory, Some(newMask))
      case ((memory, Some(curMask)), Memory(addr, nr)) =>
        val floatMask = applyMask(toBinary(addr), curMask)
        val updatedMemory = generatePermutations(floatMask).foldLeft(memory) { case (memory, bits) =>
          memory + (fromBinary(bits) -> nr)
        }
        (updatedMemory, Some(curMask))
    }
    mem.values.sum
  }

  assert(calculate(parseInput(TEST_DATA)) == 208)
  println(calculate(parseInput(Source.fromFile("./adventofcode-data/day14_input.txt").mkString)))
}
