package org.elmarweber.adventofcode2020

import scala.io.Source

object Day3 extends App {
  val TEST_MAP =
    """|..##.......
      |#...#...#..
      |.#....#..#.
      |..#.#...#.#
      |.#...##..#.
      |..#.##.....
      |.#.#.#....#
      |.#........#
      |#.##...#...
      |#...##....#
      |.#..#...#.#""".stripMargin
      .split('\n').toList

  val INPUT_MAP_FROM_FILE = Source.fromFile("./adventofcode-data/day3_input.txt").getLines().toList


  def navigate(map: List[String], position: (Int, Int) = (0, 0), vertical: Int = 1, horizontal: Int = 3): Int = {
    if (position._1 >= map.size) {
      0
    } else if (position == (0, 0)) {
      navigate(map, (vertical, horizontal), vertical, horizontal)
    } else {
      val sum = if (map(position._1)(position._2 % map(0).length) == '#') 1 else 0
      sum + navigate(map, (position._1 + vertical, position._2 + horizontal), vertical, horizontal)
    }
  }

  println(navigate(TEST_MAP))
  println(navigate(INPUT_MAP_FROM_FILE))
}
