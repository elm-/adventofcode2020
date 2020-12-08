package org.elmarweber.adventofcode2020

import scala.io.Source
import scala.util.parsing.combinator._


object Day7Step2 extends App with RegexParsers {
  val TEST_DATA =
    """shiny gold bags contain 2 dark red bags.
      |dark red bags contain 2 dark orange bags.
      |dark orange bags contain 2 dark yellow bags.
      |dark yellow bags contain 2 dark green bags.
      |dark green bags contain 2 dark blue bags.
      |dark blue bags contain 2 dark violet bags.
      |dark violet bags contain no other bags.""".stripMargin

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

  def sumBags(bagList: List[Bag], bagColor: BagColor): Int = {
    bagList.find(_.color == bagColor) match {
      case None => 0
      case Some(bag) => bag.contents.foldLeft(0) { case (sum, (count, itemColor)) =>
        sum + count + count * sumBags(bagList, itemColor)
      }
    }
  }

  println(bagList)
  println(sumBags(bagList, BagColor("gold", "shiny")))
}
