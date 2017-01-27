package eighties.h24.tools

import better.files._
import eighties.h24.generation._
import eighties.h24.space._

object PopulationGenerator extends App {
  val inputPath = File("data")
  val outputPath = File("results/population.csv")

  outputPath.delete(true)

  val cells = {
    val features = generateFeatures(inputPath.toJava, _ => true, new util.Random(42)).get.toVector
    val bounds = BoundingBox(features, IndividualFeature.location.get)
    def relocate = IndividualFeature.location.modify(BoundingBox.translate(bounds))
    Index(features.iterator.map(relocate), IndividualFeature.location.get, bounds.sideI, bounds.sideJ)
  }

  def formatFeature(feature: IndividualFeature) = Vector(feature.ageCategory, feature.sex, feature.education)

  for {
    i <- (0 until cells.sideI)
    j <- (0 until cells.sideJ)
    cell = cells.cells(i)(j)
  } {
    println(cell.size)
    outputPath << cell.map(f => formatFeature(f).mkString(",")).mkString("\n")
  }

}
