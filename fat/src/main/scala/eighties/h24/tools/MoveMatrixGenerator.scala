package eighties.h24.tools

import better.files.File
import eighties.h24.generation
import eighties.h24.population._

object MoveMatrixGenerator extends App {

  import eighties.h24.dynamic._

  def ls(c: AggregatedSocialCategory) = MoveMatrix.moves { category => category == c } composeLens MoveMatrix.location

  val path = File("../données/EGT 2010/presence semaine EGT")
  val outputPath = File("results")
  outputPath.createDirectories()

  val newMatrix = generation.flowsFromEGT(149, 132, path / "presence_semaine_GLeRoux.csv.lzma").get

  MoveMatrix.save(newMatrix, outputPath / "matrix.bin")
}
