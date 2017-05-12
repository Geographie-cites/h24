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
import eighties.h24.dynamic._
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

  val days = 300
  val gamaOpinion = 2

  val maxProbaToSwitch = 0.8
  val constraintsStrength = 0.05
  val inertiaCoefficient = 0.5
  val healthyDietReward = 0.4

  val outputPath = File("results")

  val worldFeature = WorldFeature.load(outputPath / "population.bin")

  val dataDirectory = File("../data/")
  val pathEGT = dataDirectory / "EGT 2010/presence semaine EGT"
  val distributionConstraints = dataDirectory / "initialisation_distribution_par_cat.csv"

  val healthCategory = generateHealthCategory(distributionConstraints)
  val interactionMap = generateInteractionMap(distributionConstraints)

  val world = generateWorld(worldFeature.individualFeatures, healthCategory, rng)
  val indexedWorld = Index.indexIndividuals(world, Individual.home.get)

  val timeSlices = MoveMatrix.load(outputPath / "matrix.bin")

  def mapWorld(world: World, file: File) = {
    def getValue(individual: Individual) = if (individual.healthCategory.behaviour == Healthy) 1.0 else 0.0
    //worldMapper.mapGray(world, file, getValue, 1000, 10)
    worldMapper.mapColorRGB(world, file, getValue)
  }

  def simulateOneDay(world: space.World, lapses: List[(TimeSlice, CellMatrix)], day: Int, slice: Int = 0): World = {
    lapses match {
      case Nil => world
      case (time, moveMatrix) :: t =>
        mapWorld(world, outputPath / "map" / s"${day}_${slice}.tiff")

        def moved = dynamic.moveInMoveMatrix(world, moveMatrix, time, rng)

        //def convicted = dynamic.localConviction(gamaOpinion, moved, rng)
        def convicted = dynamic.interchangeConviction(
          world,
          slice,
          interactionMap,
          maxProbaToSwitch = maxProbaToSwitch,
          constraintsStrength = constraintsStrength,
          inertiaCoefficient = inertiaCoefficient,
          healthyDietReward = healthyDietReward,
          rng
        )

        simulateOneDay(convicted, t, day, slice + 1)
    }
  }

  (1 to days).foldLeft(fixWorkPlace(world, timeSlices, rng)) {
    (w, s) => simulateOneDay(w, timeSlices.toList, s)
  }

}
