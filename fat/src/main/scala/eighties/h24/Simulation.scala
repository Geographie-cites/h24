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

import java.util.Calendar

import better.files._
import eighties.h24.dynamic._
import eighties.h24.dynamic.MoveMatrix
import eighties.h24.dynamic.MoveMatrix._
import eighties.h24.generation._
import eighties.h24.population._
import eighties.h24.space._

import scala.util.Random

object Simulation extends App {

  val seed = 42
  val rng = new Random(seed)

  val doMove = false
  val days = 5

  val maxProbaToSwitch = 0.8
  val constraintsStrength = 0.05
  val inertiaCoefficient = 0.5
  val healthyDietReward = 0.4

  val result = File("results")
  val generatedData = File("data")
  val outputPath = result / "nomove"
  outputPath.createDirectories
  println(Calendar.getInstance.getTime + " loading population")
  val worldFeature = WorldFeature.load(generatedData / "population.bin")
  val dataDirectory = File("../data/")
//  val pathEGT = dataDirectory / "EGT 2010/presence semaine EGT"
  val distributionConstraints = dataDirectory / "initialisation_distribution_per_cat.csv"
  println(Calendar.getInstance.getTime + " generating health categories")
  val healthCategory = generateHealthCategory(distributionConstraints)
  println(Calendar.getInstance.getTime + " generating interaction map")
  val interactionMap = generateInteractionMap(distributionConstraints)
  println(Calendar.getInstance.getTime + " generating world")
  val world = generateWorld(worldFeature.individualFeatures, healthCategory, rng)
  val bbox = worldFeature.originalBoundingBox
  val indexedWorld = Index.indexIndividuals(world, Individual.home.get)
  println(Calendar.getInstance.getTime + " loading move matrix")
  val timeSlices = MoveMatrix.load(generatedData / "matrix.bin")

  def mapHealth(world: World, bb: BoundingBox, file: File) = {
    def getValue(individual: Individual) = if (individual.healthCategory.behaviour == Healthy) 1.0 else 0.0
    worldMapper.mapColorRGB(world, bb, file, getValue)
  }

  def mapOpinion(world: World, bb: BoundingBox, file: File) = {
    def getValue(individual: Individual) = individual.healthCategory.opinion
    worldMapper.mapColorRGB(world, bb, file, getValue)
  }

  def byCell(day: Int, slice: Int, world: World, file: File) = {
    def cellInfo(cell: Vector[Individual]) =
      if(cell.isEmpty) List.fill(3)("0.0")
      else {
        def cellSize = cell.size
        def nbHealthy = cell.count(i => Individual.behaviour.get(i) == Healthy)
        def avgOpinion = cell.map(Individual.opinion.get).sum / cell.size
        List(cellSize.toString, nbHealthy.toString, avgOpinion.toString)
      }

    file.parent.createDirectories

    def indexed = Index.indexIndividuals(world)
    zipWithIndices[Vector[Individual]](indexed.cells).flatten.foreach { case(c, (i, j)) =>
      file << s"""$day,$slice,$i,$j,${cellInfo(c).mkString(",")}"""
    }
  }

  def byCategory(day: Int, slice: Int, world: World, file: File) = {
    def categoryInfo(category: Vector[Individual]) =
      if(category.isEmpty) List.fill(3)("0.0")
      else {
        def categorySize = category.size
        def nbHealthy = category.count(i => Individual.behaviour.get(i) == Healthy)
        def avgOpinion = category.map(Individual.opinion.get).sum / categorySize
        List(categorySize.toString, nbHealthy.toString, avgOpinion.toString)
      }

    AggregatedSocialCategory.all.foreach { cat =>
      def individualOfCategory = world.individuals.filter(_.socialCategory == cat)
      file <<
        s"""$day,$slice,${Sex.toCode(cat.sex)},${AggregatedAge.toCode(cat.age)},${AggregatedEducation.toCode(cat.education)},${categoryInfo(individualOfCategory).mkString(",")}"""
    }
  }

  val csvOutput = outputPath / "csv"
  csvOutput.createDirectories()

  val parameters = csvOutput / "parameters.csv"
  parameters < "maxProbaToSwitch,constraintsStrength,inertiaCoefficient,healthyDietReward,seed\n"
  parameters << s"$maxProbaToSwitch,$constraintsStrength,$inertiaCoefficient,$healthyDietReward,$seed"

  val cells = csvOutput / s"cells.csv"
  cells < "day,slice,x,y,effective,healthy,avgOpinion\n"

  val categories = csvOutput / s"categories.csv"
  categories < "day,slice,sex,age,educ,effective,healthy,avgOpinion\n"

  def simulateOneDay(world: space.World, bb: BoundingBox, lapses: List[(TimeSlice, CellMatrix)], day: Int, slice: Int = 0, doMove: Boolean): World = {
    lapses match {
      case Nil => world
      case (time, moveMatrix) :: t =>
        //mapHealth(world, bb, outputPath / "map" / "health" / s"${day}_${slice}.tiff")
        //mapOpinion(world, bb, outputPath / "map" / "opinion" / s"${day}_${slice}.tiff")
        byCell(day, slice, world, cells)
        byCategory(day, slice, world, categories)

        def moved = dynamic.moveInMoveMatrix(world, moveMatrix, time, rng)
        //def moved = dynamic.randomMove(world, 1.0, rng)

        //def convicted = dynamic.localConviction(gamaOpinion, moved, rng)
        def convicted = opinion.interchangeConviction(
          if (doMove) moved else world,
          slice,
          interactionMap,
          maxProbaToSwitch = maxProbaToSwitch,
          constraintsStrength = constraintsStrength,
          inertiaCoefficient = inertiaCoefficient,
          healthyDietReward = healthyDietReward,
          rng
        )

        simulateOneDay(convicted, bb, t, day, slice + 1, doMove)
    }
  }

  val populationWithMoves =
    assignFixNightLocation(assignRandomDayLocation(world, timeSlices, rng), timeSlices)

  println(Calendar.getInstance.getTime + " starting simulation")
  (1 to days).foldLeft(populationWithMoves) {
    (w, s) => {
      println(Calendar.getInstance.getTime + ": simulating one day")
      simulateOneDay(w, bbox, timeSlices.toList, s, 0, doMove)
    }
  }
}
