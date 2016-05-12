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
import eighties.generation.Feature

object Meatic extends App {

  val path = File("data")
  val rng = new Random(42)
  val steps = 1000
  val workers = 1.0

  def included(individual: Individual) =
    individual.education != Education.Schol && individual.age != Age.From0To14

  def byEducation = {
    def proportions(ed: Education) =
      AggregatedEducation(ed) match {
        case Some(AggregatedEducation.Low) => 0.30
        case Some(AggregatedEducation.Middle) => 0.40
        case Some(AggregatedEducation.High) => 0.90
        case  _ => Double.NaN
      }

    (age: Age, sex: Sex, education: Education, rng: Random) =>
      (rng.nextDouble() < proportions(education)) match {
        case true => Behaviour.Meat
        case false => Behaviour.Veggie
      }
  }

  //def randomBehaviour = Behaviour.random(0.75)

  def individuals =
    for {
      features <- generation.generateFeatures(path, rng)
    } yield
      features.
        flatMap(f => Individual(f, byEducation, rng)).
        filter(included).toVector


  /*def equipements =
    for {
      equipments <- generation.generateEquipments(path, rng)
    } yield equipments.flatMap(_.)*/

  def simulation(world: World, step: Int): World =
    if(step <= 0) world
    else {
      println(s"""${steps - step},${observable.ratioByAggregatedEducation(world, Behaviour.Meat).map(_._2).mkString(",")}""")
      val convict = logistic(0.3, 10.0, 0.5)(_)
      def afterWork = localConviction(convict, goToWork(world), rng)
      //def afterActivity = localConviction(convict, randomMove(world, rng), rng)
      //def changeCurve(meat: Double) = contact(0.8)(meat) //logistic(1.0, 2.0, 0.50)(meat)
      def afterNight = localConviction(convict, goBackHome(afterWork), rng)
      simulation(afterNight, step - 1)
    }

  //val world = assignWork(_ => rng.nextDouble() < workers, , rng)

  val world =
    assignWork(workers, generateAttractions(World(individuals.get, Vector.empty), 0.01, rng), rng)
  //randomiseLocation(world, rng)
  simulation(world, steps)

}
