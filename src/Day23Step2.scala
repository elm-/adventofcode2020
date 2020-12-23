package org.elmarweber.adventofcode2020

import scala.annotation.tailrec

object Day23Step2 extends App {
  val TEST_DATA = """389125467""".stripMargin

  def parseInput(input: String): List[Int] = {
    val S = 1 * 1000 * 1000
    val intInput = input.map(_.toString.toInt).toList
    val output = intInput ::: ((intInput.max + 1) to S).toList
    // some sanity checks, since when this breaks wastin lot's of CPU times
    assert(output.size == S)
    assert(output.max == S)
    assert(output == output.distinct)
    output
  }

  @tailrec
  def findNextIndex(cups: List[Int], i: Int): Int = {
    if (i == 0) cups.indexOf(cups.max)
    else if (cups.indexOf(i) != -1) cups.indexOf(i)
    else findNextIndex(cups, i - 1)
  }

  @tailrec
  def rearrangeResult(cups: List[Int]): List[Int] = {
    if (cups.head == 1) cups
    else rearrangeResult(cups.tail ::: List(cups.head))
  }

  @tailrec
  def calculate(cups: List[Int], turns: Int = 10 * 1000 * 1000): String = {
    if (turns % 1000 == 0) println(turns)
    if (turns == 0) return rearrangeResult(cups).drop(1).mkString("")
    val current = cups.head
    val (picked, rest) = cups.tail.splitAt(3)
    val indexDestination = findNextIndex(rest, current)
    val (before, after) = rest.splitAt(indexDestination + 1)
    val newRing = before ::: picked ::: after ::: List(current)
    calculate(newRing, turns - 1)
  }

  // 7h and lots of tortured electrons, but didn't have time today to finish a efficient implementation
  // scala list class is super slow with all the above, I'd try a native array to keep things simple, alternatively an
  // actual linked list may be an option since it is all about re-arranging
  // otherwise slow is the findNextIndex function that needs to iterate, using a map to keep an updated
  // reverse index of all number's current location would in parallel would solve that
  assert(calculate(parseInput(TEST_DATA)) == "67384529")
  println(calculate(parseInput("853192647")))
}
