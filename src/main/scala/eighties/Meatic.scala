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
import org.apache.commons.math3.analysis.function.Gaussian
import org.apache.commons.math3.distribution.NormalDistribution
import org.apache.commons.math3.random.JDKRandomGenerator

object Meatic extends App {

  val path = File("data")
  val outputPath = File("results")/"paris"
  outputPath.createDirectories()
  val rng = new Random(42)
  val steps = 1000
  val workers = 1.0
  val sigmaOpinion = 0.15
  val sigmaInitialOpinion = 0.05

  def included(individual: Individual) =
    individual.education != Education.Schol && individual.age != Age.From0To14
  def clamp(v: Double, min: Double = -1.0, max: Double = 1.0) = math.min(math.max(v, min), max)


  def byEducation = {
    def behaviour(ed: Education, random: Random) =
      AggregatedEducation(ed) match {
        case Some(AggregatedEducation.Low) => clamp(-0.30 + random.nextGaussian() * sigmaInitialOpinion)
        case Some(AggregatedEducation.Middle) => clamp(0.00 + random.nextGaussian() * sigmaInitialOpinion)
        case Some(AggregatedEducation.High) => clamp(0.30 + random.nextGaussian() * sigmaInitialOpinion)
        case _ => Double.NaN
      }
    (age: Age, sex: Sex, education: Education, rng: Random) => behaviour(education, rng)
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
      def individualOpinions =
       AggregatedEducation.all.map { ed =>
          val is = World.allIndividuals.getAll(world).filter(i => AggregatedEducation(i.education).get == ed)
          val bs = is.map(_.behaviour)
          (bs.average, bs.meanSquaredError).productIterator.mkString(",")
        }

      println(s"""${steps - step},${individualOpinions.mkString(",")}""")
      val name = s"paris-with-random-mobility-with-initial-gaussian${steps - step}.tiff"
      WorldMapper.mapColorRGB(world, outputPath / name)
      //val convict = logistic(0.3, 10.0, 0.5)(_)
      //def afterWork = localConviction(sigma, goToWork(world), rng)
      def afterActivity = localConviction(sigmaOpinion, randomMove(world, rng), rng)
      //def changeCurve(meat: Double) = contact(0.8)(meat) //logistic(1.0, 2.0, 0.50)(meat)
      def afterNight = localConviction(sigmaOpinion, goBackHome(afterActivity), rng)
      simulation(afterNight, step - 1)
    }

  //val world = assignWork(_ => rng.nextDouble() < workers, , rng)

  val world =
    assignWork(workers, generateAttractions(World(individuals.get, Vector.empty), 0.01, rng), rng)
  //randomiseLocation(world, rng)
  simulation(world, steps)

}
