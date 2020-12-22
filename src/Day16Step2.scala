package org.elmarweber.adventofcode2020

import scala.io.Source

object Day16Step2 extends App {
  val TEST_DATA = """class: 0-1 or 4-19
                    |row: 0-5 or 8-19
                    |seat: 0-13 or 16-19
                    |
                    |your ticket:
                    |11,12,13
                    |
                    |nearby tickets:
                    |3,9,18
                    |15,1,5
                    |5,14,9""".stripMargin

  case class Rule(field: String, validNumbers: List[Int])

  def parseInput(input: String): (List[Rule], List[Int], List[List[Int]]) = {
    val Array(rulesStr, myTicketStr, nearbyTicketsStr) = input.split("\n\n")
    // really have to upgrade to latest scala for regex pattern matching
    val RulePattern = "([a-z ]+): ([0-9]+)-([0-9]+) or ([0-9]+)-([0-9]+)".r
    val rules = rulesStr.split("\n").toList.map { ruleStr =>
      val matches = RulePattern.findAllMatchIn(ruleStr).next
      Rule(matches.group(1), (matches.group(2).toInt to matches.group(3).toInt).toList ::: (matches.group(4).toInt to matches.group(5).toInt).toList)
    }
    val myTicket = myTicketStr.split("\n")(1).split(",").toList.map(_.toInt)
    val nearbyTickets = nearbyTicketsStr.split("\n").toList.tail.map(_.split(",").toList.map(_.toInt))
    (rules, myTicket, nearbyTickets)
  }

  def calculate(rules: List[Rule], tickets: List[List[Int]]): List[Int] = {
    val allValidNumbers = rules.flatMap(_.validNumbers)
    val validTickets = tickets
      .filter { ticket =>
        (ticket.toSet -- allValidNumbers.toSet).isEmpty
      }
    val colSets = validTickets.foldLeft((0 to rules.size).toList.map(_ => Set.empty[Int])) { case (colSets, ticket) =>
      ticket.zipWithIndex.map { case (nr, i) =>
        colSets(i) + nr
      }
    }
    // first thought was a recursive search as a tree, backtracking through the solution space,
    // would have resulted in n! tries in worst case, to optimize find first col with lest options and work
    // way down from there to optimize search tree
    // OR realize when debugging that they are actually nicely following the pattern that there is only one solution, which makes
    // sense because AoC expects one solution, if there are multiple they'd have to simulate all of them, so taking the easy way
    val fieldOrder = rules
      .map { rule =>
        (rule.field, colSets.count(c => (c -- rule.validNumbers.toSet).isEmpty))
      } // count candidate cols, added up from 1 to nr of fields
      .sortBy(- _._2) // sort
      .zipWithIndex
      .map { case ((f, _), i) => (f, i) } // lookup index
    val resultFieldsIndices = fieldOrder.filter(_._1.startsWith("departure")).map(_._2)
    resultFieldsIndices
  }

  calculate(parseInput(TEST_DATA)._1, parseInput(TEST_DATA)._3)
  val (rules, myTicket, otherTickets) = parseInput(Source.fromFile("./adventofcode-data/day16_input.txt").mkString)
  val relFieldIdx = calculate(rules, myTicket :: otherTickets)
  println(relFieldIdx.map { i => myTicket(i).toLong }.product)
}
