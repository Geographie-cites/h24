package eighties

import eighties.generation.RasterVariate
import org.apache.commons.math3.analysis.function.Gaussian

import scala.util.Random

object opinion {
  def sigmaAdoption(current: Double, all: Vector[Double], sigma: Double, random: Random): Double = {
    val dist = new Gaussian(0.0, sigma)
    val d = all.map(x => dist.value(x - current))
    val v = new RasterVariate(d, Array(d.size))
    val index = (v.compute(random)(0) * d.size).toInt
    all(index)
  }
}

