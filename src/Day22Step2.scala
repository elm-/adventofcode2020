package org.elmarweber.adventofcode2020

import scala.io.Source

object Day22Step2 extends App {
  val TEST_DATA = """Player 1:
                    |9
                    |2
                    |6
                    |3
                    |1
                    |
                    |Player 2:
                    |5
                    |8
                    |4
                    |7
                    |10""".stripMargin

  def parseInput(input: String): (List[Int], List[Int]) = {
    def toIntList(input: String) = input.split("\n").toList.tail.map(_.toInt)
    val Array(p1, p2) = input.split("\n\n")
    (toIntList(p1), toIntList(p2))
  }

  def play(p1: List[Int], p2: List[Int], history: Set[(List[Int], List[Int])] = Set.empty, game: Int = 1, round: Int = 1): (List[Int], List[Int]) = {
    // debugging, then realizing that the ONE instruction worth following was "the winning card", not the highest card in the recursion
    //    println(s"Game ${game } Round ${round}")
    //    println(s"P1 Deck: ${p1.mkString(", ")}")
    //    println(s"P2 Deck: ${p2.mkString(", ")}")
    //    println(s"P1 Plays: ${p1.headOption}")
    //    println(s"P2 Plays: ${p2.headOption}")
    //    println("---------------------------------")
    if (history.contains((p1, p2))) return (p1, Nil)
    val updatedHistory = history + ((p1, p2))
    (p1.headOption, p2.headOption) match {
      case (None, _) | (_, None) => (p1, p2)
      case (Some(v1), Some(v2)) if v1 <= p1.tail.size && v2 <= p2.tail.size =>
        play(p1.tail.take(v1), p2.tail.take(v2), game = game + 1) match {
          case (Nil, _) => play(p1.tail, p2.tail ::: List(v2, v1), updatedHistory, game, round + 1)
          case (_, Nil) => play(p1.tail ::: List(v1, v2), p2.tail, updatedHistory, game, round + 1)
        }
      case (Some(v1), Some(v2)) if v1 > v2 => play(p1.tail ::: List(v1, v2), p2.tail, updatedHistory, game, round + 1)
      case (Some(v1), Some(v2)) if v1 < v2 => play(p1.tail, p2.tail ::: List(v2, v1), updatedHistory, game, round + 1)
      case (Some(v1), Some(v2)) if v1 == v2 => throw new IllegalStateException(s"Would be stuck at ${p1} ${p2}")
    }
  }

  def calculate(p1: List[Int], p2: List[Int]) = {
    val (finalP1, finalP2) = play(p1, p2)
    (finalP1 ::: finalP2).reverse.zipWithIndex.map { case (a, b) => a * (b + 1) }.sum
  }

  assert(calculate(parseInput(TEST_DATA)._1, parseInput(TEST_DATA)._2) == 291)
  val (p1, p2) = parseInput(Source.fromFile("./adventofcode-data/day22_input.txt").mkString)
  println(calculate(p1, p2))
}
