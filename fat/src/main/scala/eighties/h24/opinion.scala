package eighties.h24

import eighties.h24.generation._
import org.apache.commons.math3.analysis.function.Gaussian
import org.apache.commons.math3.util.FastMath

import scala.util.Random

object opinion {
  def sigmaAdoption(current: Double, all: Array[Double], sigma: Double, random: Random): Double = {
    //val dist = new Gaussian(0.0, sigma)
    //val d = all.toArray.map(x => dist.value(x - current))
    val dist = exponentialDistribution(sigma)(_)
    val d = all.map(x => dist(FastMath.abs(x - current)))
    val v = new RasterVariate(d, Array(d.size))
    val index = (v.compute(random)(0) * d.size).toInt
    all(index)
  }

  def exponentialDistribution(lambda: Double)(x: Double) =
    lambda * FastMath.exp(-lambda * x)
}

