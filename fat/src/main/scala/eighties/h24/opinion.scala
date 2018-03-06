package eighties.h24

import eighties.h24.generation._
import org.apache.commons.math3.analysis.function.Gaussian
import org.apache.commons.math3.util.FastMath
import better.files._
import eighties.h24.population._
import eighties.h24.space._

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


  object InterchangeConviction {
    def interchangeConvictionInCell(
     cell: Vector[Individual],
     timeOfDay: Int,
     interactions: Map[AggregatedSocialCategory, Interactions],
     maxProbaToSwitch: Double,
     constraintsStrength: Double,
     inertiaCoefficient: Double,
     healthyDietReward: Double,
     interpersonalInfluence: Double,
     random: Random): Vector[Individual] = {

      def booleanToDouble(b: Boolean) = if(b) 1.0 else 0.0

      def interactionProbability(individual: Individual) = timeOfDay match {
        case 0 => interactions(individual.socialCategory).breakfastInteraction
        case 1 => interactions(individual.socialCategory).lunchInteraction
        case 2 => interactions(individual.socialCategory).dinnerInteraction
      }

      def peering(cell: Vector[Individual]): (Vector[(Individual, Individual)], Vector[Individual]) = {
        val (interactingPeople, passivePeople) = cell.partition { individual => random.nextDouble() < interactionProbability(individual) }

        if(interactingPeople.size % 2 == 0) (random.shuffle(interactingPeople).grouped(2).toVector.map { case Vector(i1, i2) => (i1, i2) }, passivePeople)
        else (random.shuffle(interactingPeople).dropRight(1).grouped(2).toVector.map { case Vector(i1, i2) => (i1, i2) }, passivePeople ++ Seq(interactingPeople.last))
      }

      def dietRewardOpinion(individual: Individual) = {
        def opinion = Individual.opinion.get(individual)
        def getReward(o: Opinion): Opinion =  math.min(1.0, (1.0 + healthyDietReward) * o)
        if(individual.healthCategory.behaviour == Healthy) getReward(opinion) else opinion
      }

      def interactingOpinion(ego: Individual, partner: Option[Individual]): Opinion = partner.map(_.healthCategory.opinion).getOrElse(ego.healthCategory.opinion)

      def passiveOpinion(individual: Individual, healthRatio: Option[Double]): Opinion = healthRatio.getOrElse(Individual.opinion.get(individual))

      def updateBehaviour(individual: Individual): Individual = {
        def probaSwitchToUnhealthy = {
          val y =
            maxProbaToSwitch +
              booleanToDouble(individual.healthCategory.changeConstraints.budget) * constraintsStrength +
              booleanToDouble(individual.healthCategory.changeConstraints.time) * constraintsStrength -
              booleanToDouble(individual.healthCategory.changeConstraints.habit) * constraintsStrength

          val d = math.max(0.0, -2.0 * individual.healthCategory.opinion + 1.0)
          math.max(0.0, y * d)
        }

        def probaSwitchToHealthy = {
          val y =
            maxProbaToSwitch -
              booleanToDouble(individual.healthCategory.changeConstraints.budget) * constraintsStrength -
              booleanToDouble(individual.healthCategory.changeConstraints.time) * constraintsStrength -
              booleanToDouble(individual.healthCategory.changeConstraints.habit) * constraintsStrength

          val d = math.max(0.0, 2.0 * individual.healthCategory.opinion - 1.0)
          math.max(0.0, y * d)
        }

        Individual.behaviour.modify {
          case Healthy => if (random.nextDouble() < probaSwitchToUnhealthy) Unhealthy else Healthy
          case Unhealthy => if (random.nextDouble() < probaSwitchToHealthy) Healthy else Unhealthy
        }(individual)
      }

      // Nb: Clémentine trouve ça clair ! => C'est confirmé
      def updateIndividual(individual: Individual, healthyRatio: Option[Double], interactions: Map[Individual, Individual]) = {
        def a = inertiaCoefficient * dietRewardOpinion(individual)
        def b = interpersonalInfluence * interactingOpinion(individual, interactions.get(individual))
        def c = (1 - interpersonalInfluence) * passiveOpinion(individual, healthyRatio)
        Individual.opinion.set(a + (1 - inertiaCoefficient) * (b + c))(individual)
      }

      def newCell = {
        val healthyRatio = if(!cell.isEmpty) Some(cell.count(_.healthCategory.behaviour == Healthy).toDouble / cell.size) else None
        val (interactingPeople, passivePeople) = peering(cell)
        val interactions = (interactingPeople ++ interactingPeople.map(_.swap)).toMap
        cell.map(i => updateIndividual(i, healthyRatio, interactions)).map(updateBehaviour)
      }

      newCell
    }
  }


  def interchangeConviction(
    world: World,
    timeOfDay: Int,
    interactions: Map[AggregatedSocialCategory, Interactions],
    maxProbaToSwitch: Double,
    constraintsStrength: Double,
    inertiaCoefficient: Double,
    healthyDietReward: Double,
    interpersonalInfluence: Double,
    random: Random): World = {

    def cells = Index.indexIndividuals(world).cells.view.flatten.map {
      InterchangeConviction.interchangeConvictionInCell(
        _,
        timeOfDay,
        interactions,
        maxProbaToSwitch,
        constraintsStrength,
        inertiaCoefficient,
        healthyDietReward,
        interpersonalInfluence,
        random)
    }

    World.individuals.set(cells.flatten.toVector)(world)
  }


}

