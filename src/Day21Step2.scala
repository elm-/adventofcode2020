package org.elmarweber.adventofcode2020

import scala.io.Source

object Day21Step2 extends App {
  val TEST_DATA = """mxmxvkd kfcds sqjhc nhms (contains dairy, fish)
                    |trh fvjkl sbzzf mxmxvkd (contains dairy)
                    |sqjhc fvjkl (contains soy)
                    |sqjhc mxmxvkd sbzzf (contains fish)""".stripMargin

  case class Food(ingredients: List[String], allergens: List[String])

  def parseInput(input: String): List[Food] = {
    input.split("\n").toList.map { line =>
      val (ingredientsStr, allergensStr) = line.splitAt(line.indexOf('('))
      val ingredients = ingredientsStr.split(" ").toList
      val allergens = allergensStr.replace("(contains ", "").replace(")", "").split(", ").toList
      Food(ingredients, allergens)
    }
  }

  // prune index, those that hav exactly one we can know and remove from others step by step
  def pruneIndex(index: Map[String, Set[String]]): Map[String, String] = {
    val clearIds = index.filter(_._2.size == 1).values.map(_.head)
    if (clearIds.size == index.size) {
      index.mapValues(_.head)
    } else {
      pruneIndex(
        index.mapValues { value =>
          if (value.size == 1) value
          else value -- clearIds
        }
      )
    }
  }

  def calculate(foods: List[Food]) = {
    val allergenIndex = foods.foldLeft(Map.empty[String, Set[String]]) { case (index, Food(ingredients, allergens)) =>
      allergens.foldLeft(index) { case (index, allergen) =>
        val existing = index.getOrElse(allergen, ingredients.toSet)
        index + (allergen -> existing.intersect(ingredients.toSet))
      }
    }
    val prunedIndex = pruneIndex(allergenIndex)
    prunedIndex.toList.sortBy(_._1).map(_._2).mkString(",")
  }

  assert(calculate(parseInput(TEST_DATA)) == "mxmxvkd,sqjhc,fvjkl")
  println(calculate(parseInput(Source.fromFile("./adventofcode-data/day21_input.txt").mkString)))
}
