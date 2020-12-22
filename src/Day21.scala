package org.elmarweber.adventofcode2020

import scala.io.Source

object Day21 extends App {
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

  def calculate(foods: List[Food]) = {
    val allergenIndex = foods.foldLeft(Map.empty[String, Set[String]]) { case (index, Food(ingredients, allergens)) =>
      allergens.foldLeft(index) { case (index, allergen) =>
        val existing = index.getOrElse(allergen, ingredients.toSet)
        index + (allergen -> existing.intersect(ingredients.toSet))
      }
    }
    val ingredientsWithCandidates = allergenIndex.values.flatten.toSet
    val allIngredients = foods.flatMap(_.ingredients)
    allIngredients.count(i => !ingredientsWithCandidates.contains(i))
  }

  assert(calculate(parseInput(TEST_DATA)) == 5)
  println(calculate(parseInput(Source.fromFile("./adventofcode-data/day21_input.txt").mkString)))
}
