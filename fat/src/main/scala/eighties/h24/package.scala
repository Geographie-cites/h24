package eighties

import monocle.Traversal
import monocle.function.Each

import scala.annotation.tailrec
import scala.util.Random
import scalaz.Applicative

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

package object h24 {

  def multinomial[T](values: Seq[(T, Double)])(implicit random: Random): T = {
    @tailrec def multinomial0[T](values: List[(T, Double)])(draw: Double): T = {
      values match {
        case Nil ⇒ throw new RuntimeException("List should never be empty.")
        case (bs, _) :: Nil ⇒ bs
        case (bs, weight) :: tail ⇒
          if (draw <= weight) bs
          else multinomial0(tail)(draw - weight)
      }
    }

    val max =  values.map(_._2).sum
    val drawn = random.nextDouble() * max

    multinomial0(values.toList)(drawn)
  }

  implicit class SeqDecorator[T](s: Seq[T]) {
    def randomElement(random: Random) = {
      val size = s.size
      s(random.nextInt(size))
    }
  }

  implicit class SeqDoubleDecorator(s: Seq[Double]) {
    def average = s.sum / s.size
    def meanSquaredError = {
      val avg = s.average
      s.map { v ⇒ math.pow(v - avg, 2) }.average
    }
  }

  def clamp(v: Double, min: Double = -1.0, max: Double = 1.0) = math.min(math.max(v, min), max)

}
