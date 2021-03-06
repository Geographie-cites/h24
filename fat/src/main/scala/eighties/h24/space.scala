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
import eighties.h24.generation.IndividualFeature

import scala.collection.SeqView
import scala.collection.mutable.ArrayBuffer
import scala.reflect.ClassTag
import scala.util.Random

object space {
  type Coordinate = (Double, Double)
  type Location = (Int, Int)

  object Location {
    def lowerBound(l1: Location, l2: Location): Location = (math.min(l1._1, l2._1), math.min(l1._2, l2._2))
    def upperBound(l1: Location, l2: Location): Location = (math.max(l1._1, l2._1), math.max(l1._2, l2._2))
    def apply(i:Int, j:Int) = (i,j)
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

  def distance(l1: Location, l2: Location) = {
    val c1 = new com.vividsolutions.jts.geom.Coordinate(l1._1, l1._2)
    val c2 = new com.vividsolutions.jts.geom.Coordinate(l2._1, l2._2)
    c1.distance(c2)
  }

  object BoundingBox {
    def apply[T](content: Array[T], location: T => Location): BoundingBox = {
      val (minI, minJ) = content.view.map(location).reduceLeft(Location.lowerBound)
      val (maxI, maxJ) = content.view.map(location).reduceLeft(Location.upperBound)
      BoundingBox(minI = minI, maxI = maxI, minJ = minJ, maxJ = maxJ)
    }

    def translate(boundingBox: BoundingBox)(location: Location) = (location._1 - boundingBox.minI, location._2 - boundingBox.minJ)
    def allLocations(boundingBox: BoundingBox) =
      for {
        i <- boundingBox.minI to boundingBox.maxI
        j <- boundingBox.minJ to boundingBox.maxJ
      } yield (i, j)
  }

  case class BoundingBox(minI: Int, maxI: Int, minJ: Int, maxJ: Int) {
    def sideI = maxI - minI + 1
    def sideJ = maxJ - minJ + 1
  }

  object World {

    def apply[B](individuals: Array[Individual], attractions: Array[Attraction]): World = {
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

    def individualsVector = World.individuals composeLens arrayToVector[Individual]
    def allIndividuals = individualsVector composeTraversal each

  }

  //def arrayToVectorLens[A: Manifest] = monocle.Lens[Array[A], Vector[A]](_.toVector)(v => _ => v.toArray)
  //def array2ToVectorLens[A: Manifest] = monocle.Lens[Array[Array[A]], Vector[Vector[A]]](_.toVector.map(_.toVector))(v => _ => v.map(_.toArray).toArray)

  /* Définition d'une classe Grid, composé de vecteurs, de edges et de side*/
  @Lenses case class World(individuals: Array[Individual], attractions: Array[Attraction], originI: Int, originJ: Int, sideI: Int, sideJ: Int)
  @Lenses case class Attraction(location: Location, education: AggregatedEducation)

  object Index {

    def indexIndividuals(world: World, location: Individual => Location = Individual.location.get(_)) =
      Index[Individual](World.individuals.get(world).iterator, location, world.sideI, world.sideJ)

    def indexAttraction(world: World) =
      Index[Attraction](World.attractions.get(world).iterator, Attraction.location.get(_), world.sideI, world.sideJ)

    def apply[T: ClassTag](content: Iterator[T], location: T => Location, sideI: Int, sideJ: Int): Index[T] = {
      val cellBuffer: Array[Array[ArrayBuffer[T]]] = Array.fill(sideI, sideJ) { ArrayBuffer[T]() }

      for                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               {
        s <- content
        (i, j) = location(s)
      } cellBuffer(i)(j) += s

      Index[T](cellBuffer.map(_.map(_.toArray)), sideI, sideJ)
    }

    def getLocatedCells[T, U](index: Index[T]) =
      for {
        (l, i) <- index.cells.view.zipWithIndex
        (c, j) <- l.zipWithIndex
      } yield (c, Location(i, j))

    def allCells[T: ClassTag] = cells[T] composeIso arrayVectorIso[Array[Array[T]]] composeTraversal each composeIso arrayVectorIso[Array[T]] composeTraversal each composeIso arrayVectorIso[T]
    def allIndividuals[T: ClassTag] = allCells[T] composeTraversal each
  }

  @Lenses case class Index[T](cells: Array[Array[Array[T]]], sideI: Int, sideJ: Int)

  def generateWorld(
    features: Array[IndividualFeature],
    healthCategory: (AggregatedSocialCategory, Random) => HealthCategory,
    rng: Random) = {

    def included(feature: IndividualFeature) = Education(feature.education) != Education.Schol && Age(feature.ageCategory) != Age.From0To14

    def individuals = features.filter(included).map(f => Individual(f, healthCategory, rng))

    /*def equipements =
      for {
        equipments <- generation.generateEquipments(path, rng)
      } yield equipments.flatMap(_.)*/

    //assignWork(workerRatio, generateAttractions(World(individuals.get, Vector.empty), 0.01, rng), rng)

    World(individuals, Array.empty)
  }
}
