package eighties.h24.tools

import better.files.File
import eighties.h24.generation
import eighties.h24.space._
import eighties.h24.population._

object EGTStat extends App {

  import eighties.h24.dynamic._

  def ls(c: AggregatedCategory) =
    MoveMatrix.moves { category => AggregatedCategory(category) == c } composeLens MoveMatrix.location

  val path = File("../données/EGT 2010/presence semaine EGT")
  val outputPath = File("results")
  outputPath.createDirectories()

  val outFileRes = outputPath / "matrix.bin"

  val newMatrix = generation.flowsFromEGT(path / "presence_semaine_GLeRoux.csv.lzma").get
  
  val allMovesValue = MoveMatrix.allMoves.getAll(newMatrix).toVector
  val bb = BoundingBox[MoveMatrix.Move](allMovesValue, _._1)
  val allLocations = BoundingBox.allLocations(bb)

  def unreached =
    AggregatedCategory.all.map { ac =>
      val cat = ls(ac).getAll(newMatrix).groupBy(x => x)
      ac -> allLocations.count(l => !cat.contains(l))
    }

  unreached.foreach(println)

}
