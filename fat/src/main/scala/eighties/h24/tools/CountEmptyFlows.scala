package eighties.h24.tools

import better.files.File
import eighties.h24.dynamic._
import eighties.h24.generation._
import eighties.h24.population._
import eighties.h24.space._

import scala.util.Random

object CountEmptyFlows extends App {

  val rng = new Random(42)
  val outputPath = File("results")

  val features = WorldFeature.load(outputPath / "population.bin")

  val indexedWorld = Index[IndividualFeature](features.individualFeatures.iterator, IndividualFeature.location.get(_), features.boundingBox.sideI, features.boundingBox.sideJ)

  val moveTimeLapse = MoveMatrix.load(outputPath / "matrix.bin")

  AggregatedSocialCategory.all.map { ac =>
    ac ->
      MoveMatrix.getLocatedCells(moveTimeLapse).count { case(_, location, c) =>
        c.get(ac).map(_.isEmpty).getOrElse(false) &&
          indexedWorld.cells(location._1)(location._2).exists(f => AggregatedSocialCategory(f) == ac)
      }
  }.foreach(println)


}
