package eighties.h24.tools

import better.files.File
import eighties.h24.generation.IndividualFeature
import eighties.h24.population.ChangeConstraints
import eighties.h24.space._
import eighties.h24.WorldMapper

import scala.util.Random

object MapPopulation extends App {
  val rng = new Random(42)

  val days = 10
  val workers = 1.0
  val sigmaInitialOpinion = 0.05
  val gamaOpinion = 2
  val activityRatio = 0.3

  def features = IndividualFeature.load(File("results/population.bin"))

  def opinion(f: IndividualFeature, rng: Random) = 0.5
  def behaviour(f: IndividualFeature, rng: Random) = false
  def changeConstraints(f: IndividualFeature, rng: Random) = ChangeConstraints(habit = false, budget = false, time = false)

  val world = generateWorld(features, opinion, behaviour, changeConstraints, rng)
  val start = System.currentTimeMillis()
  WorldMapper.mapRGB(world, File("results") / "map.tiff")
  val end = System.currentTimeMillis()
  println((end - start) + " ms")
}
