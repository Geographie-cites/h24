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

import scala.reflect.ClassTag

object observable {

  def byEducation[T](b: scala.Array[Double] => T)(world: World) =
      for {
        ed <- AggregatedEducation.all
        level = world.individuals.filter(i => Individual.education.get(i)  == ed)
      } yield ed -> b(level.map(Individual.opinion.get))

  def resume(world: World) = {
    import breeze.linalg._
    import breeze.stats._
    byEducation[Array[Double]](b => Array(mean(b), scala.math.sqrt(variance(DenseVector(b: _*))), median(DenseVector(b: _*))))(world)
  }

  def saveEffectivesAsCSV(world: World, output: File) = {
    output.parent.createDirectories()
    output.delete(swallowIOExceptions = true)

    Index.getLocatedCells(Index.indexIndividuals(world)).foreach {
      case (c, l) =>
        def numbers = AggregatedSocialCategory.all.map { cat => c.count(i => i.socialCategory == cat)}
        output << s"""${l._1},${l._2},${numbers.mkString(",")}"""
    }
  }

  def moran[T](matrix: Array[Array[T]], quantity: T => Double): Double = {
    def adjacentCells(i: Int, j: Int, size: Int = 1) =
      for {
        oi ← -size to size
        oj ← -size to size
        if i != oi || j != oj
        if i + oi >= 0
        if j + oj >= 0
        if i + oi < matrix.size
        if j + oj < matrix(i + oi).size
      } yield matrix(i + oi)(j + oj)

    def localNeighbourhoodPairs =
      for {
        (cellI, (i, j)) ← zipWithIndices(matrix).flatten
        cellJ ← adjacentCells(i, j)
      } yield (cellI, cellJ, 1.0)

    val flatCells = matrix.toVector.flatten// ?
    val totalQuantity = flatCells.map(quantity).sum
    val averageQuantity = totalQuantity / flatCells.size

    def numerator =
      localNeighbourhoodPairs.map {
        case (cellI, cellJ, weight) ⇒
          val term1 = if (quantity(cellI) == 0) 0.0 else (quantity(cellI) - averageQuantity.toDouble)
          val term2 = if (quantity(cellJ) == 0) 0.0 else (quantity(cellJ) - averageQuantity.toDouble)
          weight * term1 * term2
      }.sum

    def denominator =
      flatCells.map {
        cell ⇒
          if (quantity(cell) <= 0) 0
          else math.pow(quantity(cell) - averageQuantity.toDouble, 2)
      }.sum

    val totalWeight = localNeighbourhoodPairs.map { case (_, _, weight) ⇒ weight }.sum

    if (denominator <= 0) 0
    else (flatCells.size.toDouble / totalWeight.toDouble) * (numerator / denominator)
  }


}
