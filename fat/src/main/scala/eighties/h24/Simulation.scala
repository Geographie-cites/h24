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
import eighties.h24.dynamic.MoveMatrix._
import eighties.h24.generation._
import eighties.h24.population._
import eighties.h24.space._

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

  def opinion(f: IndividualFeature, rng: Random) = 0.5
  def changeConstrains(f: IndividualFeature, rng: Random) = ChangeConstrains(habit = false, budget = false, time = false)
  
  val world = generateWorld(features, opinion, changeConstrains, rng)

  val pathEGT = File("../données/EGT 2010/presence semaine EGT")

  val moveTimeLapse = generation.flowsFromEGT(world.sideI,world.sideJ, pathEGT / "presence_semaine_GLeRoux.csv.lzma").get
  //val moveTimeLapse = MoveMatrix.noMove(world.sideI, world.sideJ)

  val workTimeMoves = moveTimeLapse.toMap.apply(workTimeSlice)
  def fixWorkPlace =
    World.allIndividuals.modify { individual =>
      dynamic.sampleDestinationInMoveMatrix(individual, workTimeMoves, rng) match {
        case Some(d) => Individual.stableDestinations.modify(_ + (workTimeSlice -> d))(individual)
        case None => Individual.stableDestinations.modify(_ + (workTimeSlice -> individual.home))(individual)
      }
    }

  def simulateOnDay(world: space.World, lapses: List[(TimeSlice, CellMatrix)]): World =
    lapses match {
      case Nil => world
      case (time, moveMatrix) :: t =>
        def moved = dynamic.moveInMoveMatrix(world, moveMatrix, time, rng)
        def convicted = dynamic.localConviction(gamaOpinion, moved, rng)
        simulateOnDay(convicted, t)
    }

  (1 to days).foldLeft(fixWorkPlace(world)) {
    (w, s) => simulateOnDay(w, moveTimeLapse.toList)
  }

}
