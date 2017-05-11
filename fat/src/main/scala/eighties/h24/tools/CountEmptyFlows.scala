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

  val features = IndividualFeature.load(outputPath / "population.bin")

  val boundingBox = BoundingBox[IndividualFeature](features, _.location)

  val indexedWorld = Index[IndividualFeature](features.iterator, IndividualFeature.location.get(_), boundingBox.sideI, boundingBox.sideJ)

  val moveTimeLapse = MoveMatrix.load(outputPath / "matrix.bin")

  AggregatedSocialCategory.all.map { ac =>
    ac ->
      MoveMatrix.getLocatedCells(moveTimeLapse).count { case(_, location, c) =>
        c.get(ac).map(_.isEmpty).getOrElse(false) && !indexedWorld.cells(location._1)(location._2).isEmpty
      }
  }.foreach(println)


}
