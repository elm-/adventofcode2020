package org.elmarweber.adventofcode2020

import akka.japi.Pair

import java.lang.Long
import scala.io.Source
import scala.collection.JavaConverters._

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

  // 7300ms
  def iterate(tiles: TileMap, turns: Int = 100): TileMap = {
    if (turns == 0) return tiles
    // pad playing field with adjacent tiles, could be made sparser by only adding tiles that actually change
    val paddedTiles = tiles.foldLeft(tiles) { case (tiles, ((x, y), t)) =>
      tiles ++ AllDirections.map { d =>
        (x + d.deltaX, y + d.deltaY) -> tiles.getOrElse((x + d.deltaX, y + d.deltaY), White)
      }.toMap
    }
    val nextTiles = paddedTiles.transform { case ((x, y), t) =>
      newTileState(countBlackTiles(paddedTiles, x, y), t)
    }
    iterate(nextTiles, turns - 1)
  }


  @inline
  def countBlackTiles(tiles: collection.mutable.HashMap[(Double, Double), Boolean], x: Double, y: Double): Int = {
    AllDirections.count { d =>
      tiles.getOrElse((x + d.deltaX, y + d.deltaY), White) == Black
    }
  }

  @inline
  def countBlackTiles(tiles: TileMap, x: Double, y: Double): Int = {
    AllDirections.count { d =>
      tiles.getOrElse((x + d.deltaX, y + d.deltaY), White) == Black
    }
  }

  @inline
  def newTileState(nrOfBlackTiles: Int, t: Boolean) = {
    if (t == Black && (nrOfBlackTiles == 0 || nrOfBlackTiles > 2)) White
    else if (t == White && nrOfBlackTiles == 2) Black
    else t
  }

  //7000ms
  def iterateSparse(tiles: TileMap, turns: Int = 100): TileMap = {
    if (turns == 0) return tiles
    val nextTiles = tiles
      .foldLeft(tiles) { case (newTiles, ((x, y), t)) =>
        val addedTiles = AllDirections
          .map { d =>
            val (nx, ny) = (x + d.deltaX, y + d.deltaY)
            if (tiles.isDefinedAt((nx, ny))) {
              None // skip, already in normal iteration
            } else {
              val newState = newTileState(countBlackTiles(tiles, nx, ny), White)
              if (newState == White) None
              else Some((nx, ny) -> Black)
            }
          }.collect {
            case Some(v) => v
          }.toMap
        newTiles ++ addedTiles + ((x, y) -> newTileState(countBlackTiles(tiles, x, y), t))
      }
      //.filter { case (_, v) => v == Black} // also made no difference, size of map doesn't impact, boils down to performance of map
    iterate(nextTiles, turns - 1)
  }


  // 300ms, profiler shows issues in Java boxing/unboxing
  def iterateMutableSet(tiles: TileMap, turns: Int = 100): TileMap = {
    var field = collection.mutable.HashSet.empty[(Double, Double)]
    tiles.foreach { case (k, v) => if (v == Black) field.add(k) }

    (1 to turns).foreach { _ =>
      val nextField = collection.mutable.HashSet.empty[(Double, Double)]
      val counts = collection.mutable.HashMap.empty[(Double, Double), Int]

      field.foreach { case (x, y) =>
        AllDirections.foreach { d =>
          val k = (x + d.deltaX, y + d.deltaY)
          counts.update(k, counts.getOrElse(k, 0) + 1)
        }
      }

      counts.foreach { case (k, count) =>
        if (newTileState(count, field.contains(k)) == Black) {
          nextField.add(k)
        }
      }

      field = nextField
    }
    field.map { v => v -> Black}.toMap
  }


  // 150 ms (has a bug somewhere I didn't want to dig into further, but executes same ops as scala version above
  // next gets really to JVM level, e.g. is Int better? and maybe the scala/java conversion take also couple of ms,
  // but order of magnitude what is possible I'd say
  def iterateMutableSetJava(tiles: TileMap, turns: Int = 100): TileMap = {
    val jMap = tiles.map { case ((x, y), v) =>
      new Pair(Double.box(x), Double.box(y)) -> Boolean.box(v)
    }.asJava
    val java = new Day24Step2J()
    val jResult = java.iterateJava(jMap, turns)
    jResult.asScala.map { case (k, v) =>
      (k.first.toDouble, k.second.toDouble) -> v.booleanValue()
    }.toMap
  }

  def calculate(input: List[List[Direction]], turns: Int = 100): Int = {
    val initTiles = input.foldLeft(Map((0.0, 0.0) -> White)) { (tiles, directions) =>
      val (x, y) = directions.foldLeft((0.0, 0.0)) { case ((x, y), d) => (x + d.deltaX, y + d.deltaY) }
      val tileStatus = tiles.getOrElse((x, y), White)
      tiles + ((x, y) -> ! tileStatus)
    }
    val start = System.currentTimeMillis()
    val result = iterateMutableSet(initTiles, turns).values.count(_ == Black)
    println(System.currentTimeMillis() - start)
    result
  }

//  assert(calculate(parseInput(TEST_DATA), 1) == 15)
//  assert(calculate(parseInput(TEST_DATA), 2) == 12)
//  assert(calculate(parseInput(TEST_DATA), 3) == 25)
//  assert(calculate(parseInput(TEST_DATA)) == 2208)
  while (true) {
    assert(calculate(parseInput(Source.fromFile("./adventofcode-data/day24_input.txt").mkString)) == 4280)
  }
}
