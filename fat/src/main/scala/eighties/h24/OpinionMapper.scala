package eighties.h24

import breeze.plot._
import eighties.h24.population._

import scala.util.Random

object OpinionMapper extends App {
  val sigmaOpinion = 0.15
  val sigmaInitialOpinion = 0.05

  val random = new Random(42)
  def clamp(v: Double, min: Double = -1.0, max: Double = 1.0) = math.min(math.max(v, min), max)
  def behaviour(ed: Education, random: Random) =
    AggregatedEducation(ed) match {
      case Some(AggregatedEducation.Low) => clamp(-0.30 + random.nextGaussian() * sigmaInitialOpinion)
      case Some(AggregatedEducation.Middle) => clamp(0.00 + random.nextGaussian() * sigmaInitialOpinion)
      case Some(AggregatedEducation.High) => clamp(0.30 + random.nextGaussian() * sigmaInitialOpinion)
      case _ => Double.NaN
    }

  def generateEducation(random: Random) = Education.apply(random.nextInt(7) + 1)

  val values = (0 until 100) map {_ => behaviour(generateEducation(random).get, random)}

  println(values)
  val f = Figure()
  val p = f.subplot(0)
  p += hist(values,20)
  p.title = "A histogram"
  f.saveas("subplots.png")
}
