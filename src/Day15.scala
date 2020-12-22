package org.elmarweber.adventofcode2020

import scala.io.Source

object Day15 extends App {
  val TEST_DATA = List(0, 3, 6)
  val FINAL_DATA = List(0, 6, 1, 7, 2, 19, 20)

  def calculate(input: List[Int], turns: Int = 2020): Int = {
    if (input.size == turns) input.head
    else calculate(input.tail.indexOf(input.head) + 1 :: input, turns)
  }

  assert(calculate(TEST_DATA.reverse) == 436)
  assert(calculate(List(3, 1, 2).reverse) == 1836)
  println(calculate(FINAL_DATA.reverse))
}
