package org.elmarweber.adventofcode2020

import akka.stream.stage.GraphStageLogic.StageActorRefNotInitializedException

import scala.io.Source
import scala.util.parsing.combinator._


object Day7 extends App with RegexParsers {
  val TEST_DATA =
    """light red bags contain 1 bright white bag, 2 muted yellow bags.
      |dark orange bags contain 3 bright white bags, 4 muted yellow bags.
      |bright white bags contain 1 shiny gold bag.
      |muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.
      |shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.
      |dark olive bags contain 3 faded blue bags, 4 dotted black bags.
      |vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
      |faded blue bags contain no other bags.
      |dotted black bags contain no other bags.""".stripMargin

  case class BagColor(color: String, variant: String)
  case class Bag(color: BagColor, contents: List[(Int, BagColor)] = Nil)

  def number = "[0-9]+".r ^^ { case str => str.toInt }
  def bagColor = "[a-z]+".r ~ " " ~ "[a-z]+".r ^^ { case v ~ _ ~ c => BagColor(c, v) }
  def bagColorCount = number ~ " " ~ bagColor ~ "(bag|bags)".r ^^ { case nr ~ _ ~ c ~ _ => (nr ,c)}
  def bagColorCountList = (bagColorCount ~ ".".?).* ^^ { case l => l.map(_._1) }
  def emptyBagColorList = "no other bags" ^^ { case _ => Nil: List[(Int, BagColor)]}
  def bag = bagColor ~ " bags contain " ~ (bagColorCountList|emptyBagColorList) ~ "." ^^ { case bc ~ _ ~ bcl ~ _ => Bag(bc, bcl)}
  def bagListP = (bag ~ "\n".?).* ^^ { case l => l.map(_._1)}


  def parseList(input: String): List[Bag] = {
    def parseColor(str: String): BagColor = str.split(" ") match { case Array(v, c) => BagColor(c, v)}
    input.split("\n").toList
      .map { line => // ugly quick fix, parser combinator didn't work
        val bagDef = line.substring(0, line.indexOf(" bags contain"))
        val subBagsDef = line.substring(line.indexOf(" bags contain") + 14, line.indexOf("."))
        val bagList = {
          if (subBagsDef == "no other bags") {
            Nil
          } else {
            subBagsDef.split(", ").toList.map { item =>
              val tokens = item.split( " ")
              (tokens(0).toInt, BagColor(tokens(2), tokens(1)))
            }

          }
        }
        Bag(parseColor(bagDef), bagList)
      }
  }



//  val bagList = parseList(TEST_DATA)
  val bagList = parseList(Source.fromFile("./adventofcode-data/day7_input.txt").mkString)

  def buildRootContains(bagRules: List[Bag], colorToAdd: BagColor): Set[Bag] = {
    val matchingBags = bagRules.filter(_.contents.exists(_._2 == colorToAdd)).toSet
    matchingBags ++ matchingBags.flatMap(b => buildRootContains(bagRules, b.color))
  }

  println(bagList)
  println(buildRootContains(bagList, BagColor("gold", "shiny")).map(_.color))
  println(buildRootContains(bagList, BagColor("gold", "shiny")).map(_.color).size)
}
