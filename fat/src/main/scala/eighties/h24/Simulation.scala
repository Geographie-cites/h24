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
import eighties.h24.opinion._
import eighties.h24.observable._

import scala.util.Random

object Simulation extends App {

  val rng = new Random(42)

  val days = 2
  val gamaOpinion = 2

  val outputPath = File("results")

  def features = IndividualFeature.load(outputPath / "population.bin")

  val dataDirectory = File("../données/")
  val pathEGT = dataDirectory / "EGT 2010/presence semaine EGT"
  val distributionConstraints = dataDirectory / "initialisation_distribution_par_cat.csv"


  val healthCategory = generateHealthCategory(distributionConstraints)
  val world = generateWorld(features, healthCategory, rng)
  val indexedWorld = Index.indexIndividuals(world, Individual.home.get)

  val moveTimeLapse = MoveMatrix.load(outputPath / "matrix.bin")
  //val moveTimeLapse = MoveMatrix.noMove(world.sideI, world.sideJ)

  val workTimeMoves = moveTimeLapse.toMap.apply(workTimeSlice)
  def fixWorkPlace =
    World.allIndividuals.modify { individual =>
      dynamic.sampleDestinationInMoveMatrix(individual, workTimeMoves, rng) match {
        case Some(d) => Individual.stableDestinations.modify(_ + (workTimeSlice -> d))(individual)
        case None => Individual.stableDestinations.modify(_ + (workTimeSlice -> individual.home))(individual)
      }
    }

  def simulateOneDay(world: space.World, lapses: List[(TimeSlice, CellMatrix)], day: Int, slice: Int = 0): World =
    lapses match {
      case Nil => world
      case (time, moveMatrix) :: t =>
        def moved = dynamic.moveInMoveMatrix(world, moveMatrix, time, rng)
        def convicted = dynamic.localConviction(gamaOpinion, moved, rng)
        //saveEffectivesAsCSV(convicted, outputPath / s"steps/world${day}_${slice}.csv")
        simulateOneDay(convicted, t, day, slice + 1)
    }

  (1 to days).foldLeft(fixWorkPlace(world)) {
    (w, s) => simulateOneDay(w, moveTimeLapse.toList, s)
  }

}
