package org.elmarweber.adventofcode2020

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Random

object Day1Part2 extends App {
  val TEST_LIST = List(1721, 979, 366, 299, 675, 1456)

  def efficientSolution(input: List[Int]): Option[Int] = {
    for (i <- input.indices) {
      for (j <- (i + 1) until input.size) {
        for (k <- input.indices) {
          if (input(i) + input(j) + input(k) == 2020) return Some(input(i) * input(j) * input(k))
        }
      }
    }
    None
  }

  def stressTest(fn: (List[Int] => Option[Int])): Unit = {
    (1 to 100).foreach { _ =>
      val randomList = Random.shuffle(TEST_LIST)
      val checksum = fn(randomList).getOrElse(throw new AssertionError(s"No Result for ${randomList}"))
      assert(checksum == 241861950)
    }
  }

  assert(efficientSolution(TEST_LIST) == Some(241861950))

  stressTest(efficientSolution)

  val dayTestData = Source.fromFile("./adventofcode-data/day1_input.txt").getLines().map(_.toInt).toList
  println(efficientSolution(dayTestData))
}
