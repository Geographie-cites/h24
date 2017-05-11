package eighties.h24.tools

import better.files.File
import eighties.h24.generation._
import eighties.h24.population.AggregatedSocialCategory
import eighties.h24.space._

import scala.util.Random

object CellCSV extends App {
  val rng = new Random(42)

  def features = IndividualFeature.load(File("results/population.bin"))

  val dataDirectory = File("../données/")
  val pathEGT = dataDirectory / "EGT 2010/presence semaine EGT"
  val distributionConstraints = dataDirectory / "initialisation_distribution_par_cat.csv"

  val healthCategory = generateHealthCategory(distributionConstraints)

  val world = Index.indexIndividuals(generateWorld(features, healthCategory, rng))

  val output = File("results") / "cells.csv"
  output.delete(swallowIOExceptions = true)

  Index.getLocatedCells(world).foreach {
    case (c, l) =>
      def numbers = AggregatedSocialCategory.all.map { cat => c.count(i => AggregatedSocialCategory(i.socialCategory) == cat)}
      output << s"""${l._1},${l._2},${numbers.mkString(",")}"""
  }

}
