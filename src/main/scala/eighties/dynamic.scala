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
package eighties

import population._
import space._
import monocle.Monocle._

import scala.util.Random

object dynamic {

  type Move = (World, Random) => Individual => Individual

  def reachable(index: Index) =
    for {
      i <- 0 until index.sideI
      j <- 0 until index.sideJ
      if !index.cells(i)(j).isEmpty
    } yield (i, j)

  def randomMove(world: World, random: Random) = {
    val reach = reachable(Index(world))
    val rSize = reach.size
    def randomLocation = Individual.location.modify(_ => reach(random.nextInt(rSize)))
    (World.allIndividuals modify randomLocation) (world)
  }

  def goBackHome(world: World) = {
    def m = (individual: Individual) => Individual.location.set(individual.home)(individual)
    (World.allIndividuals modify m)(world)
  }

  def goToWork(world: World) = {
    def m = (individual: Individual) =>
      Individual.location.set(individual.work.getOrElse(individual.location))(individual)
    (World.allIndividuals modify m)(world)
  }

  type Conviction = Vector[Individual] => Vector[Individual]

  def localConviction(proba: Double => Double, world: World, random: Random) = {
    def newCells =
      Index.allCells.modify { cell =>
        if (cell.isEmpty) cell
        else {
          val probaByGroup = cell.groupBy(_.behaviour).mapValues(_.size.toDouble / cell.size).mapValues(proba).withDefaultValue(0.0)
          cell applyTraversal (each[Vector[Individual], Individual] composeLens Individual.behaviour) modify { b =>
            b match {
              case Behaviour.Meat => if(random.nextDouble() < probaByGroup(Behaviour.Veggie)) Behaviour.Veggie else b
              case Behaviour.Veggie => if(random.nextDouble() < probaByGroup(Behaviour.Meat)) Behaviour.Meat else b
            }
          }
        }
      }

    def newIndividuals = Index.allIndividuals.getAll(newCells(Index(world)))
    World.individuals.set(newIndividuals.toVector)(world)
  }

  def assignWork(worker: Individual => Boolean, world: World, random: Random) = {
    val reach = reachable(Index(world))
    val reachSize = reach.size
    def assign(individual: Individual): Individual =
      if(worker(individual)) Individual.work.set(Some(reach(random.nextInt(reachSize)))) (individual)
      else individual

    (World.allIndividuals modify assign)(world)
  }


  def logistic(l: Double, k: Double, x0: Double)(x: Double) = l / (1.0 +  math.exp(-k * (x - x0)))

  //def contact(teta: Double)(x: Double) = if(x < teta) 0.0 else 1.0

}
