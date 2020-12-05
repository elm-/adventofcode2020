package org.elmarweber.adventofcode2020

import scala.io.Source

object Day5 extends App {
  def decode(input: String): (Int, Int, Int) = {
    val (rowStr, colStr) = input.splitAt(7)
    val binaryRowStr = rowStr.replace('F', '0').replace('B', '1')
    val binaryColStr = colStr.replace('L', '0').replace('R', '1')
    val row = Integer.parseInt(binaryRowStr, 2)
    val col = Integer.parseInt(binaryColStr, 2)
    (row, col, row * 8 + col)
  }

  assert(decode("BFFFBBFRRR") == (70, 7, 567))
  assert(decode("FFFBBBFRRR") == (14, 7, 119))
  assert(decode("BBFFBBFRLL") == (102, 4, 820))

  val maxInput = Source.fromFile("./adventofcode-data/day5_input.txt").getLines().map(decode).map(_._3).max
  println(maxInput)
}
