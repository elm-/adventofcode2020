package org.elmarweber.adventofcode2020

import scala.io.Source

object Day3Step2 extends App {
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

  println(navigate(TEST_MAP, (0, 0), 1, 1))
  println(navigate(TEST_MAP, (0, 0), 1, 3))
  println(navigate(TEST_MAP, (0, 0), 1, 5))
  println(navigate(TEST_MAP, (0, 0), 1, 7))
  println(navigate(TEST_MAP, (0, 0), 2, 1))


  println(navigate(INPUT_MAP_FROM_FILE, (0, 0), 1, 1))
  println(navigate(INPUT_MAP_FROM_FILE, (0, 0), 1, 3))
  println(navigate(INPUT_MAP_FROM_FILE, (0, 0), 1, 5))
  println(navigate(INPUT_MAP_FROM_FILE, (0, 0), 1, 7))
  println(navigate(INPUT_MAP_FROM_FILE, (0, 0), 2, 1))

  println(
    navigate(INPUT_MAP_FROM_FILE, (0, 0), 1, 1).toLong
  * navigate(INPUT_MAP_FROM_FILE, (0, 0), 1, 3).toLong
  * navigate(INPUT_MAP_FROM_FILE, (0, 0), 1, 5).toLong
  * navigate(INPUT_MAP_FROM_FILE, (0, 0), 1, 7).toLong
  * navigate(INPUT_MAP_FROM_FILE, (0, 0), 2, 1).toLong
  )
}