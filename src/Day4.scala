package org.elmarweber.adventofcode2020

import scala.util.parsing.combinator._
import scala.io.Source

object Day4 extends App with RegexParsers {
  val testData =
    """ecl:gry pid:860033327 eyr:2020 hcl:#fffffd
      |byr:1937 iyr:2017 cid:147 hgt:183cm
      |
      |iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884
      |hcl:#cfa07d byr:1929
      |
      |hcl:#ae17e1 iyr:2013
      |eyr:2024
      |ecl:brn pid:760753108 byr:1931
      |hgt:179cm
      |
      |hcl:#cfa07d eyr:2025 pid:166559648
      |iyr:2011 ecl:brn hgt:59in""".stripMargin

  val expectedItems = Set(
    "byr",
    "iyr",
    "eyr",
    "hgt",
    "hcl",
    "ecl",
    "pid"
  )

  def countValid(input: String): Int = {
    val items = input.split("\n\n")
    items
      .count { item =>
        val tokens = item.split("( |\n)+").toList
        val map = tokens.map(_.split(':').toList).map(x => (x(0), x(1))).toMap
        (expectedItems -- map.keys).isEmpty
      }
  }
  println(countValid(testData))
  println(countValid(Source.fromFile("./adventofcode-data/day4_input.txt").mkString))
}
