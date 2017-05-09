package eighties.h24.tools

import better.files.File
import eighties.h24.generation
import eighties.h24.space._
import eighties.h24.population._

object EGTStat extends App {

  import eighties.h24.dynamic._

  val ls =
    MoveMatrix.allMoves /*moves { category => AggregatedCategory(category) == AggregatedCategory(AggregatedAge.Senior, Sex.Female, AggregatedEducation.Low) }*/ composeLens MoveMatrix.location

  val path = File("../données/EGT 2010/presence semaine EGT")
  val outputPath = File("results")
  outputPath.createDirectories()

  val outFileRes = outputPath / "matrix.bin"

  val newMatrix = generation.flowsFromEGT(path / "presence_semaine_GLeRoux.csv.lzma").get


 MoveMatrix.cells.getAll(newMatrix).toSeq.flatMap { _.keys.map(AggregatedCategory.apply) }.distinct.foreach(println)

  val allMovesValue = MoveMatrix.allMoves.getAll(newMatrix).toVector
  val bb = BoundingBox[MoveMatrix.Move](allMovesValue, _._1)
  val cat = ls.getAll(newMatrix).groupBy(x => x)
  println(BoundingBox.allLocations(bb).count { l => cat.contains(l) })


}
