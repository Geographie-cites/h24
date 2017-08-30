package eighties.h24.tools

import better.files.File
import eighties.h24.generation._
import eighties.h24.population._
import eighties.h24.space._

import scala.util.Random

object MoveMatrixGenerator extends App {

  import eighties.h24.dynamic._

  def ls(c: AggregatedSocialCategory) = MoveMatrix.moves { category => category == c } composeLens MoveMatrix.location

  val path = File("../data/EGT 2010/presence semaine EGT")
  val outputPath = File("results")
  outputPath.createDirectories()

  val bb = WorldFeature.load(File("results/population2.bin")).originalBoundingBox
  println(bb.minI + " " + bb.minJ + " " + bb.sideI + " " + bb.sideJ)

  val newMatrix = flowsFromEGT(bb, path / "presence_semaine_GLeRoux.csv.lzma").get
  MoveMatrix.save(newMatrix, outputPath / "matrix2.bin")
}
