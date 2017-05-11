package eighties.h24.tools

import better.files.File
import eighties.h24.generation._
import eighties.h24.population._
import eighties.h24.space._
import eighties.h24.{WorldMapper, generation}

import scala.util.Random

object MapPopulation extends App {
  val rng = new Random(42)

  val days = 10
  val workers = 1.0
  val sigmaInitialOpinion = 0.05
  val gamaOpinion = 2
  val activityRatio = 0.3

  def features = IndividualFeature.load(File("results/population.bin"))

  val dataDirectory = File("../données/")
  val distributionConstraints = dataDirectory / "initialisation_distribution_par_cat.csv"

  val healthCategory = generation.generateHealthCategory(distributionConstraints)

  val world = generateWorld(features, healthCategory, rng)
  WorldMapper.mapRGB(world, File("results") / "map.tiff")
}
