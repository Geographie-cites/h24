package eighties

import monocle.{Optional, Traversal}
import monocle.function.{Each, Index}
import monocle.macros.Lenses

import scala.annotation.tailrec
import scala.util.Random
import scalaz.Traverse

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
  class Multinomial[T](values: List[(T, Double)]) {
    @tailrec private def multinomial0[T](values: List[(T, Double)])(draw: Double): T = {
      values match {
        case Nil ⇒ throw new RuntimeException("List should never be empty.")
        case (bs, _) :: Nil ⇒ bs
        case (bs, weight) :: tail ⇒
          if (draw <= weight) bs
          else multinomial0(tail)(draw - weight)
      }
    }
    val max =  values.map(_._2).sum
    def draw(implicit random: Random): T = {
      val drawn = random.nextDouble() * max
      multinomial0(values)(drawn)
    }
  }

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
  implicit class ArrayDecorator[T](s: Array[T]) {
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

  def zipWithIndices[T](matrix: Vector[Vector[T]]): Vector[Vector[(T, (Int, Int))]] =
    matrix.zipWithIndex.map { case(line, i) => line.zipWithIndex.map { case(c, j) => (c, (i, j)) } }

  def rescale(min: Double, max: Double, value: Double) = min + value * (max - min)

  /*
  implicit val traverseArray: Traverse[Array] = new Traverse[Array] {

//    override def traverseImpl[G[_], A, B](fa: Array[A])(f: A => G[B])(implicit evidence$1: Applicative[G]): G[Array[B]] =
//      fa.traverse(f)
    override def traverse[G[_], A, B](fa: Array[A])(f: A => G[B])(implicit evidence$1: Applicative[G]): G[Array[B]] = ???
    override def foldLeft[A, B](fa: Array[A], b: B)(f: (B, A) => B): B = ???

    override def foldRight[A, B](fa: Array[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = ???
  }
  */
  /*
  implicit def arrayIndex[A]: Index[Array[A], Int, A] = new Index[Array[A], Int, A] {
    def index(i: Int) =
      Optional[Array[A], A](v =>
        if(v.isDefinedAt(i)) Some(v(i))     else None)(a => v =>
        if(v.isDefinedAt(i)) v.updated(i,a) else v)
  }
  */
  //implicit def arrayEach[A]: Each[Array[A], A] = fromTraverse

  /*
  implicit def traversalArray[T] = new Traversal[Array[T], T] {
    override def modifyF[F[_]](f: T => F[T])(s: Array[T])(implicit evidence$1: Applicative[F]): F[Array[T]] =
      s.traverse(f)
  }

  implicit def eachArray[T] = new Each[Array[T], T] {
    override def each: Traversal[Array[T], T] = traversalArray[T]
  }
  */

}
