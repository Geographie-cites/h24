/**
  * Created by Romain Reuillon on 09/05/16.
  *
  * This program is free software: you can redistribute it and/or modify
  * it under the terms of the GNU Affero General Public License as published by
  * the Free Software Foundation, either version 3 of the License, or
  * (at your option) any later version.
  *
  * This program is distributed in the hope that it will be useful,
  * but WITHOUT ANY WARRANTY; without even the implied warranty of
  * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  * GNU General Public License for more details.
  *
  * You should have received a copy of the GNU General Public License
  * along with this program.  If not, see <http://www.gnu.org/licenses/>.
  *
  */
package eighties.h24

import java.io.FileOutputStream
import java.nio.file.Files
import java.util.zip.GZIPOutputStream

import better.files._
import eighties.h24.dynamic.MoveMatrix
import eighties.h24.dynamic.MoveMatrix._
import eighties.h24.generation._
import eighties.h24.population._
import eighties.h24.space._
import org.apache.commons.compress.compressors.bzip2.BZip2CompressorOutputStream

import scala.util.Random

object Simulation extends App {

  val path = File("data")
  val outputPath = File("results") / "paris"
  outputPath.createDirectories()

  val rng = new Random(42)
  val days = 10
  val workers = 1.0
  val sigmaInitialOpinion = 0.05
  val gamaOpinion = 2
  val activityRatio = 0.3

  def features = IndividualFeature.load(File("results/population.bin"))
  val world = generateWorld(features, (_,_) => 0.5, rng)

  val pathEGT = File("../données/EGT 2010/presence semaine EGT")
//  val outputPath = File("results")
//  outputPath.createDirectories()

  //val outFileRes = outputPath / "matrix.bin"

  val moveTimeLapse = generation.flowsFromEGT(world.sideI,world.sideJ, pathEGT / "presence_semaine_GLeRoux.csv.lzma").get
  //val moveTimeLapse = MoveMatrix.noMove(world.sideI, world.sideJ)

  //println(world.sideI -> world.sideJ)

//  def simulateOnDay(world: space.World, lapses: List[(TimeSlice, CellMatrix)]) = {
//
//  }

  val last =
    (1 to days).foldLeft(world) {
      (w, s) =>

        def nw = dynamic.moveInMoveMatrix(w, moveTimeLapse.head._2, rng)
        dynamic.localConviction(gamaOpinion, nw, rng)
    }

  
}
