package org.elmarweber.adventofcode2020

import scala.annotation.tailrec
import scala.io.Source

object Day23 extends App {
  val TEST_DATA = """389125467""".stripMargin

  def parseInput(input: String): List[Int] = input.map(_.toString.toInt).toList

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

  // not sure if a ring buffer could make this simpler, since lot of operations need to be with an index
  @tailrec
  def calculate(cups: List[Int], turns: Int = 100): String = {
    if (turns == 0) return rearrangeResult(cups).drop(1).mkString("")
    val current = cups.head
    val (picked, rest) = cups.tail.splitAt(3)
    val indexDestination = findNextIndex(rest, current)
    val (before, after) = rest.splitAt(indexDestination + 1)
    val newRing = before ::: picked ::: after ::: List(current)
    calculate(newRing, turns - 1)
  }

  assert(calculate(parseInput(TEST_DATA)) == "67384529")
  println(calculate(parseInput("853192647")))
}
