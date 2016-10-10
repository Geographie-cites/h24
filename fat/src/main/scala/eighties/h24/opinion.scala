package eighties.h24

import eighties.h24.generation._
import org.apache.commons.math3.analysis.function.Gaussian
import org.apache.commons.math3.util.FastMath

import scala.util.Random

object opinion {

  def sigmaAdoption(current: Double, all: Array[Double], sigma: Double, random: Random): Double = {
    val dist = new Gaussian(0.0, sigma)
    val d = all.map(x => dist.value(x - current))
    val v = new RasterVariate(d, Array(d.size))
    val index = (v.compute(random)(0) * d.size).toInt
    all(index)
  }

  def binomialAdoption(current: Double, all: Array[Double], gama: Double, random: Random): Double = {
    val other = all(random.nextInt(all.size))
    val distance = math.abs(current - other)

    val p = 1 / FastMath.pow(1 + distance, gama)
    if(random.nextDouble() <= p) other else current
  }

}

