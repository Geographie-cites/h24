package eighties.h24.tools

import better.files._
import eighties.h24.generation._
import eighties.h24.space._


object PopulationGenerator extends App {
  def features = generateFeatures(File("data").toJava, _ => true, new util.Random(42)).get.toVector
  IndividualFeature.save(features, File("results/population.csv.gz"))
}
