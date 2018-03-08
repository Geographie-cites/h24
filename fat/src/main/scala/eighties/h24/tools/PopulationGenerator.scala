package eighties.h24.tools

import java.util.Calendar

import better.files._
import eighties.h24.generation._
import eighties.h24.space._


object PopulationGenerator extends App {
  println(Calendar.getInstance.getTime + " Generating population")
  val features = generateFeatures(
    File("data").toJava,
    _ => true,
    new util.Random(42),
    generatePopulation2
  ).get.toVector
  println(Calendar.getInstance.getTime + " Relocating population")
  val originalBoundingBox = BoundingBox(features, IndividualFeature.location.get)
  def relocate = IndividualFeature.location.modify(BoundingBox.translate(originalBoundingBox))
  val relocatedFeatures = features.map(relocate)
  println(Calendar.getInstance.getTime + " Saving population")
  val boundingBox = BoundingBox[IndividualFeature](relocatedFeatures, _.location)
  WorldFeature.save(
    WorldFeature(relocatedFeatures, originalBoundingBox, boundingBox),
    File("results/population2.bin")
  )
}
