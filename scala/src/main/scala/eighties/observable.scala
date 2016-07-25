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

import eighties.population.{AggregatedEducation, Behaviour, Education}
import space._

object observable {

  def ratioOfPopulation(world: World, behaviour: Behaviour) =
    world.individuals.count(_.behaviour == behaviour).toDouble / world.individuals.size


  def ratioByEducation(world: World, behaviour: Behaviour) =
    for {
      ed <- Education.all
      level = world.individuals.filter(i => i.education == ed)
      behave = level.filter(i => i.behaviour == behaviour)
    } yield ed -> (behave.size.toDouble / level.size)


  def ratioByAggregatedEducation(world: World, behaviour: Behaviour) =
    for {
      ed <- AggregatedEducation.all
      level = world.individuals.filter(i => AggregatedEducation(i.education).map(_ == ed).getOrElse(false))
      behave = level.filter(i => i.behaviour == behaviour)
    } yield ed -> (behave.size.toDouble / level.size)


}
