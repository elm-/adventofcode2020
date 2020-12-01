package org.elmarweber.adventofcode2020

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Random

object Day1 extends App {

  type Result = (Int, Int, Int)

  val TEST_LIST = List(1721, 979, 366, 299, 675, 1456)

  def naiveSolution(input: List[Int]): Option[Result] = {
    input.foreach { x =>
      input.foreach { y =>
        if (x + y == 2020) return Some((x, y, x * y))
      }
    }
    None
  }

  def efficientSolution(input: List[Int]): Option[Result] = {
    for (i <- input.indices) {
      for (j <- (i + 1) until input.size) {
        if (input(i) + input(j) == 2020) return Some((input(i), input(j), input(i) * input(j)))
      }
    }
    None
  }

  def funcTailRecursive(input: List[Int]): Option[Result] = funcTailRecursive(input, None) // for stress test function

  @tailrec
  def funcTailRecursive(input: List[Int], output: Option[Result]= None): Option[Result] = (input, output) match {
    case (_, Some(result)) => Some(result)
    case (_ :: Nil, _) => None
    case (x :: tail, _) =>
      val result = tail.foldLeft(None: Option[Result]) {
        case (None, y) if x + y == 2020 => Some((x, y, x * y))
        case (r, _) => r
      }
      funcTailRecursive(tail, result)
  }

  def stressTest(fn: (List[Int] => Option[Result])): Unit = {
    (1 to 100).foreach { _ =>
      val randomList = Random.shuffle(TEST_LIST)
      val (x, y, checksum) = fn(randomList).getOrElse(throw new AssertionError(s"No Result for ${randomList}"))
      assert(((x == 1721|| y == 299) || (x == 299 || y == 1721)) && (checksum == 514579))
    }
  }

  assert(naiveSolution(TEST_LIST) == Some((1721, 299, 514579)))
  assert(efficientSolution(TEST_LIST) == Some((1721, 299, 514579)))
  assert(funcTailRecursive(TEST_LIST) == Some((1721, 299, 514579)))

  stressTest(naiveSolution)
  stressTest(efficientSolution)
  stressTest(funcTailRecursive)

  val dayTestData = Source.fromFile("./adventofcode-data/day1_input.txt").getLines().map(_.toInt).toList
  println(naiveSolution(dayTestData))
}
