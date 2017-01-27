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

  object Location {
    def lowerBound(l1: Location, l2: Location): Location = (math.min(l1._1, l2._1), math.min(l1._2, l2._2))
    def upperBound(l1: Location, l2: Location): Location = (math.max(l1._1, l2._1), math.max(l1._2, l2._2))
  }

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

  def cell(p: Coordinate) = ((p._1 / 1000.0).toInt, (p._2 / 1000.0).toInt)


  object BoundingBox {
    def apply[T](content: Vector[T], location: T => Location): BoundingBox = {
      val (minI, minJ) = content.view.map(location).reduceLeft(Location.lowerBound)
      val (maxI, maxJ) = content.view.map(location).reduceLeft(Location.upperBound)
      BoundingBox(minI = minI, maxI = maxI, minJ = minJ, maxJ = maxJ)
    }

    def translate(boundingBox: BoundingBox)(location: Location) = (location._1 - boundingBox.minI, location._2 - boundingBox.minJ)
  }

  case class BoundingBox(minI: Int, maxI: Int, minJ: Int, maxJ: Int) {
    def sideI = maxI - minI + 1
    def sideJ = maxJ - minJ + 1
  }

  object World {

    def apply(individuals: Vector[Individual], attractions: Vector[Attraction]): World = {
      val boundingBox = BoundingBox(individuals, Individual.location.get)

      def relocate =
        Individual.home.modify(BoundingBox.translate(boundingBox)) andThen
          Individual.location.modify(BoundingBox.translate(boundingBox))

      World(
        individuals.map(relocate),
        attractions,
        boundingBox.minI,
        boundingBox.minJ,
        boundingBox.sideI,
        boundingBox.sideJ)
    }

    def allIndividuals = World.individuals composeTraversal each

  }

  /* Définition d'une classe Grid, composé de vecteurs, de edges et de side*/
  @Lenses case class World(individuals: Vector[Individual], attractions: Vector[Attraction], originI: Int, originJ: Int, sideI: Int, sideJ: Int)
  @Lenses case class Attraction(location: Location, education: AggregatedEducation)

  object Index {

    def indexIndividuals(world: World) =
      Index[Individual](World.individuals.get(world).iterator, Individual.location.get(_), world.sideI, world.sideJ)

    def indexAttraction(world: World) =
      Index[Attraction](World.attractions.get(world).iterator, Attraction.location.get(_), world.sideI, world.sideJ)

    def apply[T](content: Iterator[T], location: T => Location, sideI: Int, sideJ: Int): Index[T] = {
      val cellBuffer: Array[Array[ArrayBuffer[T]]] = Array.fill(sideI, sideJ) { ArrayBuffer[T]() }

      for {
        s <- content
        (i, j) = location(s)
      } cellBuffer(i)(j) += s

      Index[T](cellBuffer.toVector.map(_.toVector.map(_.toVector)), sideI, sideJ)
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

    //assignWork(workerRatio, generateAttractions(World(individuals.get, Vector.empty), 0.01, rng), rng)
    World(individuals.get, Vector.empty)
  }
}
