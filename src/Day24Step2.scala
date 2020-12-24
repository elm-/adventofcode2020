package org.elmarweber.adventofcode2020

import scala.io.Source

object Day24Step2 extends App {
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

  def AllDirections = List(East, SouthEast, SouthWest, West, NorthWest, NorthEast)


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

  type TileMap = Map[(Double, Double), Boolean]

  def White = false
  def Black = true

  def iterate(tiles: TileMap, turns: Int = 100): TileMap = {
    if (turns == 0) return tiles
    // pad playing field with adjacent tiles, could be made sparser by only adding tiles that actually change
    val paddedTiles = tiles.foldLeft(tiles) { case (tiles, ((x, y), t)) =>
      tiles ++ AllDirections.map { d =>
        (x + d.deltaX, y + d.deltaY) -> tiles.getOrElse((x + d.deltaX, y + d.deltaY), White)
      }.toMap
    }
    val nextTiles = paddedTiles.transform { case ((x, y), t) =>
      val nrOfBlackTiles = AllDirections.count { d =>
        tiles.getOrElse((x + d.deltaX, y + d.deltaY), White) == Black
      }
      if (t == Black && (nrOfBlackTiles == 0 || nrOfBlackTiles > 2)) White
      else if (t == White && nrOfBlackTiles == 2) Black
      else t
    }
    iterate(nextTiles, turns - 1)
  }

  def calculate(input: List[List[Direction]], turns: Int = 100): Int = {
    val initTiles = input.foldLeft(Map((0.0, 0.0) -> White)) { (tiles, directions) =>
      val (x, y) = directions.foldLeft((0.0, 0.0)) { case ((x, y), d) => (x + d.deltaX, y + d.deltaY) }
      val tileStatus = tiles.getOrElse((x, y), White)
      tiles + ((x, y) -> ! tileStatus)
    }
    iterate(initTiles, turns).values.count(_ == Black)
  }

  assert(calculate(parseInput(TEST_DATA), 1) == 15)
  assert(calculate(parseInput(TEST_DATA), 2) == 12)
  assert(calculate(parseInput(TEST_DATA), 3) == 25)
  assert(calculate(parseInput(TEST_DATA)) == 2208)
  println(calculate(parseInput(Source.fromFile("./adventofcode-data/day24_input.txt").mkString)))
}
