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
package eighties

import better.files._
import population._
import scala.util.Random
import space._
import dynamic._

object Meatic extends App {

  val path = File("data")
  val rng = new Random(42)

  def individuals =
    for {
      features <- generation.generateFeatures(path, rng)
    } yield features.flatMap(f => Individual(f, Behaviour.random(0.5), rng)).toVector



  def simulation(world: World, step: Int): World =
    if(step <= 0) world
    else {
      println(s"$step ${observable.ratioOfPopulation(world, Behaviour.Meat)}")
      def day = move(world, randomMove(0.5), rng)
      def afterDay = localConviction(day, rng)
      def night = move(afterDay, backHome, rng)
      def afterNight = localConviction(night, rng)
      simulation(afterNight, step - 1)
    }

  val world = World(individuals.get)
  println(simulation(world, 100))

  /*for {
    e <- Education.all
  } println(e -> grid.individuals.count(_.education == e))*/

}
