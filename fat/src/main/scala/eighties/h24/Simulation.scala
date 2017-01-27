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
package eighties.h24

import better.files._
import eighties.h24.dynamic._
import eighties.h24.population._
import eighties.h24.space._
import shapeless.Lazy

import scala.util.Random

object Simulation extends App {

  val path = File("data")
  val outputPath = File("results") / "paris"
  outputPath.createDirectories()

  val rng = new Random(42)
  val steps = 10
  val workers = 1.0
  val sigmaInitialOpinion = 0.05
  val gamaOpinion = 2
  val activityRatio = 0.3

  val h24 = new H24(gamaOpinion, activityRatio)

  def world = generateWorld(path.toJava, _ => true /*_.startsWith("75")*/, sigmaInitialOpinion, workers, rng)

 //println(observable.resume(world))
//
//  def save(w: World, s: Int) = {
//    val name = s"paris-with-random-mobility-with-initial-gaussian${s}.tiff"
//    WorldMapper.mapColorRGB(w, outputPath / name toJava)
//  }


  //save(world, 0)

  val last =
    (1 to steps).foldLeft(world) {
      (w, s) =>
        println(s"begin $s at " + System.currentTimeMillis())
        val nw = h24.simulation(world, 1, rng)
        println(s"end $s at " + System.currentTimeMillis())
        //println(observable.resume(nw))
        nw
    }
}
