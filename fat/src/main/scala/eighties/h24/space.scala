/**
  * Created by Romain Reuillon on 10/05/16.
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

import population._
import dynamic._
import monocle.Monocle._
import monocle.macros._
import better.files._

import scala.collection.mutable.ArrayBuffer
import scala.util.Random

object space {
  type Coordinate = (Double, Double)
  type Location = (Int, Int)

  /* définition d'un voisinage*/
  def neighbours(side: Int, location: Location, size: Int) = {
    val (i, j) = location
    for {
      di <- (-size to size)
      dj <- (-size to size)
      if (di != 0 && dj != 0)
      ni = i + di
      nj = j + dj
      if (ni >= 0 && nj >= 0 && ni < side && nj < side)
    } yield (i + di, j + dj)
  }

  def cell(p: Coordinate) = ((p._1 / 200.0).toInt, (p._2 / 200.0).toInt)

  object World {

    def apply(individuals: Vector[Individual], attractions: Vector[Attraction]): World = {
      val minI = individuals.map(Individual.i.get).min
      val maxI = individuals.map(Individual.i.get).max
      val minJ = individuals.map(Individual.j.get).min
      val maxJ = individuals.map(Individual.j.get).max
      val sideI = maxI - minI
      val sideJ = maxJ - minJ

      def translate(location: Location) = (location._1 - minI, location._2 -minJ)
      def relocate = Individual.home.modify(translate) andThen Individual.location.modify(translate)
      def relocated = individuals.map(relocate)

      World(relocated, attractions, minI, minJ, sideI + 1, sideJ + 1)
    }

    def allIndividuals = World.individuals composeTraversal each

  }

  /* Définition d'une classe Grid, composé de vecteurs, de edges et de side*/
  @Lenses case class World(individuals: Vector[Individual], attractions: Vector[Attraction], originI: Int, originJ: Int, sideI: Int, sideJ: Int)
  @Lenses case class Attraction(location: Location, education: AggregatedEducation)

  object Index {

    def indexIndividuals(world: World) =
      Index[Individual](world, World.individuals.get(_), Individual.location.get(_))

    def indexAttraction(world: World) =
      Index[Attraction](world, World.attractions.get(_), Attraction.location.get(_))

    def apply[T](world: World, select: World => Vector[T], location: T => Location): Index[T] = {
      val cellBuffer: Array[Array[ArrayBuffer[T]]] = Array.fill(world.sideI, world.sideJ) { ArrayBuffer[T]() }

      for {
        s <- select(world)
        (i, j) = location(s)
      } cellBuffer(i)(j) += s

      Index[T](cellBuffer.toVector.map(_.toVector.map(_.toVector)), world.sideI, world.sideJ)
    }

    def allCells[T] = cells[T] composeTraversal each composeTraversal each
    def allIndividuals[T] = allCells[T] composeTraversal each
  }

  @Lenses case class Index[T](cells: Vector[Vector[Vector[T]]], sideI: Int, sideJ: Int)

  def generateWorld(
    path: java.io.File,
    filter: String => Boolean,
    sigmaInitialOpinion: Double,
    workerRatio: Double,
    rng: Random) = {

    def included(individual: Individual) = individual.education != Education.Schol && individual.age != Age.From0To14

    def byEducation = {
      def behaviour(ed: Education, random: Random) =
        AggregatedEducation(ed) match {
          case Some(AggregatedEducation.Low) => (-1.0 + random.nextGaussian() * sigmaInitialOpinion)
          case Some(AggregatedEducation.Middle) => (0.0 + random.nextGaussian() * sigmaInitialOpinion)
          case Some(AggregatedEducation.High) => (1.0 + random.nextGaussian() * sigmaInitialOpinion)
          case _ => Double.NaN
        }
      (age: Age, sex: Sex, education: Education, rng: Random) => behaviour(education, rng)
    }

    def individuals =
      for {
        features <- generation.generateFeatures(path, filter, rng)
      } yield features.flatMap(f => Individual(f, byEducation, rng)).filter(included).toVector

    /*def equipements =
      for {
        equipments <- generation.generateEquipments(path, rng)
      } yield equipments.flatMap(_.)*/

    assignWork(workerRatio, generateAttractions(World(individuals.get, Vector.empty), 0.01, rng), rng)

  }
}
