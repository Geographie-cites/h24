package eighties.generation

import scala.collection.mutable.ArrayBuffer
import scala.util.Random
import scalaz._
import Scalaz._

class RasterVariate(pdf: Seq[Double], val m_size: Seq[Int]) {
  val N = m_size.size
  val m_totsize = m_size.product
  val m_cdf = buildCdf(m_totsize, pdf, m_size).toParArray
  val m_sum = pdf.foldLeft(0.0)((a, b) => a + b)

  def buildCdf(totsize: Int, pdf: Seq[Double], size: Seq[Int]) = {
    var sum = 0.0
    var cdf = ArrayBuffer(0.0)
    for (i <- 0 until totsize) {
      sum = sum + pdf(i)
      cdf.append(sum)
    }
    cdf = cdf.map(_ / sum)
    cdf.toIndexedSeq
  }

  def compute(rng: Random): Vector[Double] = {
    def dim(i: Int) = State[(Int, Random), Double] { case(offset, rng) =>
      val ix = offset % m_size(i)
      val newOffset = offset / m_size(i)
      val v = (ix + rng.nextDouble()) / m_size(i)
      ((newOffset, rng), v)
    }

    val x = rng.nextDouble()
    val offset = m_cdf.indexWhere(p => p > x) - 1

    (0 until N).toVector.map(dim).sequenceU.eval((offset, rng))
  }
}