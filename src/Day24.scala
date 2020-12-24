package org.elmarweber.adventofcode2020

import scala.annotation.tailrec
import scala.io.Source

object Day24 extends App {
  val TEST_DATA = """sesenwnenenewseeswwswswwnenewsewsw
                    |neeenesenwnwwswnenewnwwsewnenwseswesw
                    |seswneswswsenwwnwse
                    |nwnwneseeswswnenewneswwnewseswneseene
                    |swweswneswnenwsewnwneneseenw
                    |eesenwseswswnenwswnwnwsewwnwsene
                    |sewnenenenesenwsewnenwwwse
                    |wenwwweseeeweswwwnwwe
                    |wsweesenenewnwwnwsenewsenwwsesesenwne
                    |neeswseenwwswnwswswnw
                    |nenwswwsewswnenenewsenwsenwnesesenew
                    |enewnwewneswsewnwswenweswnenwsenwsw
                    |sweneswneswneneenwnewenewwneswswnese
                    |swwesenesewenwneswnwwneseswwne
                    |enesenwswwswneneswsenwnewswseenwsese
                    |wnwnesenesenenwwnenwsewesewsesesew
                    |nenewswnwewswnenesenwnesewesw
                    |eneswnwswnwsenenwnwnwwseeswneewsenese
                    |neswnwewnwnwseenwseesewsenwsweewe
                    |wseweeenwnesenwwwswnew""".stripMargin


  sealed trait Direction { def deltaX: Double ; def deltaY: Double }
  case object East extends Direction { def deltaX = 1 ; def deltaY = 0 }
  case object SouthEast extends Direction { def deltaX = +0.5 ; def deltaY = -0.5 }
  case object SouthWest extends Direction { def deltaX = -0.5 ; def deltaY = -0.5 }
  case object West extends Direction { def deltaX = -1 ; def deltaY = 0 }
  case object NorthEast extends Direction { def deltaX = +0.5 ; def deltaY = 0.5 }
  case object NorthWest extends Direction { def deltaX = -0.5 ; def deltaY = 0.5 }
  object Direction {
    def fromPrefixString(str: String): (String, Direction) = {
      if (str.startsWith("sw")) (str.stripPrefix("sw"), SouthWest)
      else if (str.startsWith("se")) (str.stripPrefix("se"), SouthEast)
      else if (str.startsWith("nw")) (str.stripPrefix("nw"), NorthWest)
      else if (str.startsWith("ne")) (str.stripPrefix("ne"), NorthEast)
      else if (str.startsWith("e")) (str.stripPrefix("e"), East)
      else if (str.startsWith("w")) (str.stripPrefix("w"), West)
      else throw new IllegalArgumentException(s"Cannot parse ${str}")
    }
  }

  def parseInput(input: String): List[List[Direction]] = {
    input.split("\n").toList.map { line =>
      // yeah, there is a cleaner way
      var str = line
      var directions = List.empty[Direction]
      while (str.nonEmpty) {
        val (newStr, direction) = Direction.fromPrefixString(str)
        str = newStr
        directions = directions ::: List(direction)
      }
      directions
    }
  }


  def calculate(input: List[List[Direction]]): Int = {
    // false = white, true = black
    val tiles = input.foldLeft(Map((0.0, 0.0) -> false)) { (tiles, directions) =>
      val (x, y) = directions.foldLeft((0.0, 0.0)) { case ((x, y), d) => (x + d.deltaX, y + d.deltaY) }
      val tileStatus = tiles.getOrElse((x, y), false)
      tiles + ((x, y) -> ! tileStatus)
    }
    tiles.values.count(_ == true)
  }


  assert(calculate(parseInput(TEST_DATA)) == 10)
  println(calculate(parseInput(Source.fromFile("./adventofcode-data/day24_input.txt").mkString)))
}
