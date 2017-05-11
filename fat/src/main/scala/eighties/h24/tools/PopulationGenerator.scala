package eighties.h24.tools

import better.files._
import eighties.h24.generation._
import eighties.h24.space._


object PopulationGenerator extends App {
  def features = generateFeatures(File("data").toJava, _ => true, new util.Random(42)).get.toVector
  val boundingBox = BoundingBox(features, IndividualFeature.location.get)
  def relocate = IndividualFeature.location.modify(BoundingBox.translate(boundingBox))
  val relocatedFeatures = features.map(relocate)
  IndividualFeature.save(relocatedFeatures, File("results/population.bin"))
}
