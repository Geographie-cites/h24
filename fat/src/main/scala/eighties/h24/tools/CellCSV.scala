package eighties.h24.tools

import better.files.File
import eighties.h24.generation._
import eighties.h24.population.AggregatedSocialCategory
import eighties.h24.space._
import eighties.h24.observable._

import scala.util.Random

object CellCSV extends App {
  val rng = new Random(42)

  def features = IndividualFeature.load(File("results/population.bin"))

  val dataDirectory = File("../données/")
  val pathEGT = dataDirectory / "EGT 2010/presence semaine EGT"
  val distributionConstraints = dataDirectory / "initialisation_distribution_par_cat.csv"

  val healthCategory = generateHealthCategory(distributionConstraints)
  val world = generateWorld(features, healthCategory, rng)

  val output = File("results") / "cells.csv"

  saveEffectivesAsCSV(world, output)

}
