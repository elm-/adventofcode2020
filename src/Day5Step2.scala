package org.elmarweber.adventofcode2020

import scala.io.Source

object Day5Step2 extends App {
  def decode(input: String): (Int, Int, Int) = {
    val (rowStr, colStr) = input.splitAt(7)
    val binaryRowStr = rowStr.replace('F', '0').replace('B', '1')
    val binaryColStr = colStr.replace('L', '0').replace('R', '1')
    val row = Integer.parseInt(binaryRowStr, 2)
    val col = Integer.parseInt(binaryColStr, 2)
    (row, col, row * 8 + col)
  }

  val knownSeats = Source.fromFile("./adventofcode-data/day5_input.txt").getLines().map(decode).toList
  val validSets = knownSeats
    .filter { case (row, _, _) =>
      row != 0 && row != 127
    }
  val mySeats = validSets
    .map(_._3)
    .sorted
    .grouped(2)
    .filter { case List(id1, id2) =>
      id2 - id1 != 1
    }
  println(mySeats.toList)
}
