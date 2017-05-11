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

import better.files.File
import eighties.h24.population._
import eighties.h24.space._
import breeze.linalg._
import breeze.stats._
import eighties.h24.tools.CellCSV.world

object observable {

  def byEducation[T](b: scala.Vector[Double] => T)(world: World) =
      for {
        ed <- AggregatedEducation.all
        level = world.individuals.filter(i => AggregatedEducation(Individual.education.get(i))  == ed)
      } yield ed -> b(level.map(Individual.opinion.get))

//  def medianByEducation = byEducation { v => median(DenseVector(v: _*)) }
//  def mseByEducation = byEducation(b => scala.math.sqrt(variance(b)))
//  def meanByEducation = byEducation { v => mean(v) }

  def resume(world: World) =
    byEducation[Vector[Double]](b => Vector(mean(b), scala.math.sqrt(variance(DenseVector(b: _*))), median(DenseVector(b: _*))))(world)

  def saveEffectivesAsCSV(world: World, output: File) = {
    output.parent.createDirectories()
    output.delete(swallowIOExceptions = true)

    Index.getLocatedCells(Index.indexIndividuals(world)).foreach {
      case (c, l) =>
        def numbers = AggregatedSocialCategory.all.map { cat => c.count(i => AggregatedSocialCategory(i.socialCategory) == cat)}
        output << s"""${l._1},${l._2},${numbers.mkString(",")}"""
    }
  }

}
