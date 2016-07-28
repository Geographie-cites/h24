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
import org.saddle._

object observable {

  def byEducation[T](b: Vec[Behaviour] => T) =
    (world: World) =>
      for {
        ed <- AggregatedEducation.all
        level = world.individuals.filter(i => AggregatedEducation(i.education).map(_ == ed).getOrElse(false))
      } yield ed -> b(level.map(_.behaviour).toVec)

  def medianByEducation = byEducation(_.median)
  def mseByEducation = byEducation(b => math.sqrt(b.variance))
  def meanByEducation = byEducation(_.mean)

  def resume =
    byEducation(b => Seq(b.mean, math.sqrt(b.variance), b.median))


}
