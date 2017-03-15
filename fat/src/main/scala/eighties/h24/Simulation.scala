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

import better.files._
import eighties.h24.dynamic.MoveMatrix
import eighties.h24.generation._
import eighties.h24.population._
import eighties.h24.space._

import scala.util.Random

object Simulation extends App {

  val path = File("data")
  val outputPath = File("results") / "paris"
  outputPath.createDirectories()

  val rng = new Random(42)
  val steps = 10
  val workers = 1.0
  val sigmaInitialOpinion = 0.05
  val gamaOpinion = 2
  val activityRatio = 0.3

  def features = IndividualFeature.load(File("results/population.csv.gz"))
  val world = generateWorld(features, (_,_) => 0.5, rng)

  val moveTimeLapse = MoveMatrix.noMove(world.sideI, world.sideJ)

  val last =
    (1 to steps).foldLeft(world) {
      (w, s) =>
        def nw = dynamic.moveInMoveMatrix(w, moveTimeLapse, rng)
        dynamic.localConviction(gamaOpinion, nw, rng)
    }
}
