package org.elmarweber.adventofcode2020

import scala.io.Source

object Day16 extends App {
  val TEST_DATA = """class: 1-3 or 5-7
                    |row: 6-11 or 33-44
                    |seat: 13-40 or 45-50
                    |
                    |your ticket:
                    |7,1,14
                    |
                    |nearby tickets:
                    |7,3,47
                    |40,4,50
                    |55,2,20
                    |38,6,12""".stripMargin

  case class Rule(field: String, validNumbers: List[Int])

  def parseInput(input: String): (List[Rule], List[Int], List[List[Int]]) = {
    val Array(rulesStr, myTicketStr, nearbyTicketsStr) = input.split("\n\n")
    // really have to upgrade to latest scala for regex pattern matching
    val RulePattern = "([a-z]+): ([0-9]+)-([0-9]+) or ([0-9]+)-([0-9]+)".r
    val rules = rulesStr.split("\n").toList.map { ruleStr =>
      val matches = RulePattern.findAllMatchIn(ruleStr).next
      Rule(matches.group(1), (matches.group(2).toInt to matches.group(3).toInt).toList ::: (matches.group(4).toInt to matches.group(5).toInt).toList)
    }
    val myTicket = myTicketStr.split("\n")(1).split(",").toList.map(_.toInt)
    val nearbyTickets = nearbyTicketsStr.split("\n").toList.tail.map(_.split(",").toList.map(_.toInt))
    (rules, myTicket, nearbyTickets)
  }

  def calculate(rules: List[Rule], tickets: List[List[Int]]): Int = {
    val allValidNumbers = rules.flatMap(_.validNumbers)
    tickets
      .map { ticket =>
        ticket.toSet -- allValidNumbers.toSet
      }
      .map(_.sum).sum
  }

  assert(calculate(parseInput(TEST_DATA)._1, parseInput(TEST_DATA)._3) == 71)
  assert(calculate(parseInput(Source.fromFile("./adventofcode-data/day16_input.txt").mkString)._1, parseInput(Source.fromFile("./adventofcode-data/day16_input.txt").mkString)._3) == 71)
}
