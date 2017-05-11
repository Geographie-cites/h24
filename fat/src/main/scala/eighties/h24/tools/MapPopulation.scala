package eighties.h24.tools

import better.files.File
import eighties.h24.generation._
import eighties.h24.population._
import eighties.h24.space._
import eighties.h24.{worldMapper, generation}

import scala.util.Random

object MapPopulation extends App {
  val rng = new Random(42)
  def features = IndividualFeature.load(File("results/population.bin"))

  val dataDirectory = File("../data")
  val distributionConstraints = dataDirectory / "initialisation_distribution_par_cat.csv"

  val healthCategory = generation.generateHealthCategory(distributionConstraints)

  val world = generateWorld(features, healthCategory, rng)
  val start = System.currentTimeMillis()
//  worldMapper.mapRGB(world, File("results") / "map.tiff")
  def getValue(individual: Individual) = if (individual.healthCategory.behaviour == Healthy) 1.0 else 0.0
  worldMapper.mapGray(world, File("results") / "map.tiff", getValue, 1000, 10)
  val end = System.currentTimeMillis()
  println((end - start) + " ms")
}

// % de gens sains par cellule
// moyenne de l'opinion par cellule

// par rapport à l'état initial : nb de personnes qui ont changé
// par rapport à l'état initial : % de personnes qui ont changé
// par rapport à l'état initial : % de personnes qui sont devenus sains
// par rapport à l'état initial : % de personnes qui sont devenus malsains
