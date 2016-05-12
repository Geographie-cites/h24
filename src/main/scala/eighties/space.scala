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
package eighties

import com.vividsolutions.jts.geom.Point
import eighties.population.Individual
import monocle.Monocle._
import monocle.macros._

import scala.collection.mutable.ArrayBuffer

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
    def apply(individuals: Vector[Individual]): World = {
      val minI = individuals.map(Individual.i.get).min
      val maxI = individuals.map(Individual.i.get).max
      val minJ = individuals.map(Individual.j.get).min
      val maxJ = individuals.map(Individual.j.get).max
      val sideI = maxI - minI
      val sideJ = maxJ - minJ

      def translate(location: Location) = (location._1 - minI, location._2 -minJ)
      def relocate = Individual.home.modify(translate) andThen Individual.location.modify(translate)
      def relocated = individuals.map(relocate)

      World(relocated, sideI + 1, sideJ + 1)
    }

    def allIndividuals = World.individuals composeTraversal each
  }

  /* Définition d'une classe Grid, composé de vecteurs, de edges et de side*/
  @Lenses case class World(individuals: Vector[Individual], sideI: Int, sideJ: Int)


  object Index {

    def apply(world: World): Index = {
      val cellBuffer: Array[Array[ArrayBuffer[Individual]]] = Array.fill(world.sideI, world.sideJ) { ArrayBuffer[Individual]() }

      for {
        individual <- world.individuals
      } cellBuffer(Individual.i.get(individual))(Individual.j.get(individual)) += individual

      Index(cellBuffer.toVector.map(_.toVector.map(_.toVector)), world.sideI, world.sideJ)
    }

    def allCells = cells composeTraversal each composeTraversal each
    def allIndividuals = allCells composeTraversal each

  }

  @Lenses case class Index(cells: Vector[Vector[Vector[Individual]]], sideI: Int, sideJ: Int)
  //@Lenses case class Cell(location: Location, individuals: Vector[Individual])
}
