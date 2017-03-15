/**
  * Created by Romain Reuillon on 11/05/16.
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

import eighties.h24.population._
import eighties.h24.space._
import monocle.Monocle._
import squants._

import scala.util.Random

object dynamic {

  type Move = (World, Random) => Individual => Individual

  def reachable[T](index: Index[T]) =
    for {
      i <- 0 until index.sideI
      j <- 0 until index.sideJ
      if !index.cells(i)(j).isEmpty
    } yield (i, j)

  def randomMove(world: World, ratio: Double, random: Random) = {
    val reach = reachable(Index.indexIndividuals(world))
    val rSize = reach.size
    def randomLocation =
      Individual.location.modify(l => if(random.nextDouble() < ratio) reach(random.nextInt(rSize)) else l)

    (World.allIndividuals modify randomLocation) (world)
  }

  def goBackHome(world: World) = {
    def m = (individual: Individual) => Individual.location.set(individual.home)(individual)
    (World.allIndividuals modify m)(world)
  }

  object MoveMatrix {
    type Moves = Vector[TimeLapse]
    type TimeLapse = Vector[Vector[Cell]]
    type Cell = Map[Category, Vector[Move]]
    case class Move(location: Location, flow: Double)
    case class Category(age: Age, sex: Sex, education: Education)
  }

  def moveSampledInEGT(world: World, moves: MoveMatrix.Moves, time: Time, random: Random) = {
    def sampleMoveInEGT(individual: Individual) = {
      Individual.location.set(individual.home)(individual)
    }
    (World.allIndividuals modify sampleMoveInEGT)(world)
  }

  def localConviction(gama: Double, world: World, random: Random) = {
    def cs = Index.allCells[Individual].getAll(Index.indexIndividuals(world))

    def newIndividuals =
      cs.map { cell =>
        val size = cell.size
        if (size == 0) cell
        else {
          //val cellBehaviours = random.shuffle(cell.map(_.behaviour)).take((size * 0.01).toInt + 1).toArray
          val cellBehaviours = cell.map(_.behaviour).toArray
          cell applyTraversal (each[Vector[Individual], Individual] composeLens Individual.behaviour) modify { b: Double =>
            opinion.binomialAdoption(b, cellBehaviours, gama, random)
          }
        }
      }

    World.individuals.set(newIndividuals.flatten.toVector)(world)
  }


//  def assignWork(proportion: Double, world: World, random: Random) = {
//    val attractions =
//      AggregatedEducation.all.map {ed =>
//        ed -> world.attractions.filter(_.education == ed)
//      }.toMap.withDefaultValue(Seq.empty)
//
//    def assign(individual: Individual): Individual =
//      if(random.nextDouble() < proportion) {
//        val individualAttractions = attractions(AggregatedEducation(individual.education).get)
//        if (individualAttractions.isEmpty) individual
//        else Individual.work.set(Some(individualAttractions.randomElement(random).location)) (individual)
//      } else individual
//
//    (World.allIndividuals modify assign)(world)
//  }

  def randomiseLocation(world: World, random: Random) = {
    val reach = reachable(Index[Individual](world.individuals.iterator, Individual.location.get(_), world.sideI, world.sideJ))
    val reachSize = reach.size

    def assign(individual: Individual): Individual =
      Individual.home.set(reach(random.nextInt(reachSize))) (individual)

    (World.allIndividuals modify assign)(world)
  }

  def generateAttractions(world: World, proportion: Double, random: Random) = {
    val reach = reachable(Index.indexIndividuals(world))

    def attraction = {
      val location = reach.randomElement(random)
      val education = AggregatedEducation.all.randomElement(random)
      Attraction(location, education)
    }

    def attractions = (0 until (reach.size * proportion).toInt).map(_ => attraction)
    World.attractions.set(attractions.toVector)(world)
  }

  def logistic(l: Double, k: Double, x0: Double)(x: Double) = l / (1.0 +  math.exp(-k * (x - x0)))

  //def contact(teta: Double)(x: Double) = if(x < teta) 0.0 else 1.0

}
