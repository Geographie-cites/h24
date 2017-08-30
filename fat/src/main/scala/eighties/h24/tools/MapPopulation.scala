package eighties.h24.tools

import better.files.File
import eighties.h24.generation._
import eighties.h24.population._
import eighties.h24.space._
import eighties.h24.{worldMapper, generation}

import scala.util.Random

object MapPopulation extends App {
  val rng = new Random(42)
  val popFile = "population.bin"
  val result = "results"
  def features = WorldFeature.load(File("results") / popFile)

  val dataDirectory = File("../data")
  val distributionConstraints = dataDirectory / "initialisation_distribution_per_cat.csv"

  val healthCategory = generation.generateHealthCategory(distributionConstraints)

  val world = generateWorld(features.individualFeatures, healthCategory, rng)
  val bb = features.originalBoundingBox
  def filter(n:Int) = n>=30
//  worldMapper.mapRGB(world, File("results") / "map.tiff")
  def getHealthyValue(individual: Individual) = if (individual.healthCategory.behaviour == Healthy) 1.0 else 0.0
  //worldMapper.mapGray(world, File("results") / "map.tiff", getValue, 1000, 10)
  worldMapper.mapColorRGB(world, bb, File(result) / "healthFilter30.tiff", getHealthyValue,filter)

  def getOpinionValue(individual: Individual) = individual.healthCategory.opinion
  worldMapper.mapColorRGB(world, bb, File(result) / "opinionFilter30.tiff", getOpinionValue,filter)

  def getPop(individual: Individual) = 1.0
  worldMapper.mapColorRGB(world, bb, File(result) / "popFilter30.tiff", getPop,filter, v=>v.sum, 0.0, 10000.0)
}

// % de gens sains par cellule
// moyenne de l'opinion par cellule

// par rapport à l'état initial : nb de personnes qui ont changé
// par rapport à l'état initial : % de personnes qui ont changé
// par rapport à l'état initial : % de personnes qui sont devenus sains
// par rapport à l'état initial : % de personnes qui sont devenus malsains

// par cellule
// propo agent sain
// moyenne opinions
// propo agent devenus sains depuis ini
// propo agents devenus pas sains