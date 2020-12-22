package org.elmarweber.adventofcode2020

import scala.annotation.tailrec
import scala.collection.mutable

object Day15Step2 extends App {
  val TEST_DATA = List(0, 3, 6)
  val FINAL_DATA = List(0, 6, 1, 7, 2, 19, 20)

  def calculate(input: List[Int], turns: Int): Int = {
    val mem = new mutable.HashMap[Int, Int]()
    input.slice(0, input.size - 1).zipWithIndex.foreach { case (n, i) => mem.put(n, i + 1) }
    var current = input.last
    var turn = input.size
    while (turn < turns) {
      if (!mem.isDefinedAt(current)) {
        mem.put(current, turn)
        current = 0
      } else {
        val newNumber = turn - mem(current)
        mem.put(current, turn)
        current = newNumber
      }
      turn += 1
    }
    current
  }

  assert(calculate(TEST_DATA, 2020) == 436)
  assert(calculate(List(3, 1, 2), 2020) == 1836)
  val start = System.currentTimeMillis()
  println(calculate(FINAL_DATA, 30 * 1000 * 1000))
  println("R", System.currentTimeMillis() - start)
  val start2 = System.currentTimeMillis()
  println(calculate(FINAL_DATA, 30 * 1000 * 1000))
  println(calculate(FINAL_DATA, 30 * 1000 * 1000))
  println(calculate(FINAL_DATA, 30 * 1000 * 1000))
  println(calculate(FINAL_DATA, 30 * 1000 * 1000))
  println(calculate(FINAL_DATA, 30 * 1000 * 1000))
  println("R", (System.currentTimeMillis() - start) / 5)
}
