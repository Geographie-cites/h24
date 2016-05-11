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

  def randomLocation(i: Int, j: Int, random: Random) =
    ((i * random.nextDouble()).toInt, (j * random.nextDouble()).toInt)

  def randomMove(ratio: Double)(world: World, random: Random) =
    Individual.location.set(randomLocation(world.sideI, world.sideJ, random))

  def backHome(world: World, random: Random) =
    (individual: Individual) => Individual.location.set(individual.home)(individual)

  def move(world: World, move: Move, random: Random) =
    (World.individuals composeTraversal each).modify(move(world, random))(world)

  type Conviction = Vector[Individual] => Vector[Individual]

  def localConviction(world: World, random: Random) = {
    def newCells =
      Index(world).cells.map { line =>
        line.map { cell =>
          if(cell.isEmpty) cell
          else {
            val ratios = cell.groupBy(i => i.behaviour).mapValues(_.size.toDouble).toSeq
            cell applyTraversal (each[Vector[Individual], Individual] composeLens Individual.behaviour) set (multinomial(ratios)(random))
          }
        }
      }
    World.individuals.set(newCells.flatten.flatten)(world)
  }

}
