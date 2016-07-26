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

object Simulation extends App {

  val path = File("data")
  val outputPath = File("results") / "paris"
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

  val world =
    assignWork(workers, generateAttractions(World(individuals.get, Vector.empty), 0.01, rng), rng)

  val h24 = new H24(sigmaOpinion)

  def save(w: World, s: Int) = {
     val name = s"paris-with-random-mobility-with-initial-gaussian${s}.tiff"
        WorldMapper.mapColorRGB(w, outputPath / name)
  }

  save(world, 0)

  (1 to 100).foldLeft(world) {
    (w, s) =>
      val nw = h24.simulation(world, 10, rng)
      save(w, s)
      nw
  }



}
