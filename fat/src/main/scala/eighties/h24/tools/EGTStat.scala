package eighties.h24.tools

import better.files.File
import eighties.h24.generation
import eighties.h24.space._
import eighties.h24.population._

object EGTStat extends App {

  import eighties.h24.dynamic._

  val ls = MoveMatrix.moves(Category(Age.From30To44, Sex.Female, Education.BACP2)) composeLens MoveMatrix.location

  val path = File("../données/EGT 2010/presence semaine EGT")
  val outputPath = File("results")
  outputPath.createDirectories()

  val outFileRes = outputPath / "matrix.bin"

  generation.flowsFromEGT(path / "presence_semaine_GLeRoux.csv.lzma").foreach {
    newMatrix =>
      val allMovesValue = MoveMatrix.allMoves.getAll(newMatrix).toVector
      val bb = BoundingBox[MoveMatrix.Move](allMovesValue, _._1)
      val cat = ls.getAll(newMatrix).groupBy(x => x)
      println(BoundingBox.allLocations(bb).count { l => cat.contains(l) })
  }

}
