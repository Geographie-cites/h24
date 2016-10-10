/**
  * Created by Romain Reuillon on 26/07/16.
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

import eighties.h24.dynamic._
import eighties.h24.space._

import scala.util.Random

object H24 {

}

class H24(gamaOpinion: Double, activityRatio: Double) {

  def simulation(world: World, steps: Int, rng: Random): World = {
    def simulation0(world: World, step: Int): World =
      if (step >= steps) world
      else {
        /*def individualOpinions =
         AggregatedEducation.all.map { ed =>
            val is = World.allIndividuals.getAll(world).filter(i => AggregatedEducation(i.education).get == ed)
            val bs = is.map(_.behaviour)
            (bs.average, bs.meanSquaredError).productIterator.mkString(",")
          }*/

        //println(s"""${steps - step},${individualOpinions.mkString(",")}""")
       //val convict = logistic(0.3, 10.0, 0.5)(_)
        //def changeCurve(meat: Double) = contact(0.8)(meat) //logistic(1.0, 2.0, 0.50)(meat)

        def afterWork = localConviction(gamaOpinion, goToWork(world), rng)
        def afterActivity = localConviction(gamaOpinion, randomMove(afterWork, activityRatio, rng), rng)
        def afterNight = localConviction(gamaOpinion, goBackHome(afterActivity), rng)

        simulation0(afterNight, step + 1)
      }

    simulation0(world, 0)
  }

}