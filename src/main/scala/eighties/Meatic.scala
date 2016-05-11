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

import better.files.File
import com.vividsolutions.jts.geom.Point
import eighties.population.Individual

import scala.util.Random
import space._

object Meatic extends App {

  val path = File("data")
  val rng = new Random(42)

  def individuals =
    for {
      features <- generation.generateFeatures(path, rng)
    } yield features.flatMap(f => Individual(f)).toVector

  val grid = World(individuals.get)

  val begin = System.currentTimeMillis()
  Index(grid)
  println(System.currentTimeMillis() - begin)



}
