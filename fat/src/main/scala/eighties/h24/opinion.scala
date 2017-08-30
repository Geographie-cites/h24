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

      def dietReward(individual: Individual) = {
        def getReward(o: Opinion): Opinion =  math.min(1.0, (1.0 + healthyDietReward) * o)
        if(individual.healthCategory.behaviour == Healthy) (Individual.healthCategory composeLens HealthCategory.opinion modify getReward) (individual)
        else individual
      }

      def opinionInertia(previousOpinion: Opinion, opinion: Opinion): Opinion =
        previousOpinion * inertiaCoefficient + (1.0 - inertiaCoefficient) * opinion

      def updateInteractingOpinion(ego: Individual, partner: Individual, healthRatio: Option[Double]): Individual = {
        val influencePartner = (booleanToDouble(partner.healthCategory.behaviour == Healthy) + partner.healthCategory.opinion) / 2
        Individual.opinion.modify { o =>
          healthRatio match {
            case Some(hr) => opinionInertia(o, 0.5 * influencePartner + 0.5 * hr)
            case None => o // This wont happend
          }
        }(ego)
      }

      def updatePassiveOpinion(individual: Individual, healthRatio: Option[Double]): Individual = Individual.opinion.modify { o =>
        healthRatio match {
          case Some(hr) => opinionInertia(o, 0.5 * o + 0.5 * hr)
          case None => o
        }
      }(individual)


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

      def newCell = {
        val healthyRatio = if(!cell.isEmpty) Some(cell.count(_.healthCategory.behaviour == Healthy).toDouble / cell.size) else None
        val rewarded = cell.map(dietReward)
        val (interactingPeople, passivePeople) = peering(rewarded)

        val sens1 = interactingPeople.map { case(ego, partner) => updateInteractingOpinion(ego, partner, healthyRatio) }
        val sens2 = interactingPeople.map { case(partner, ego) => updateInteractingOpinion(ego, partner, healthyRatio)  }

        val afterInteractions = sens1 ++ sens2

        val afterPassiveInteractions = passivePeople.map(i => updatePassiveOpinion(i, healthyRatio))

        (afterInteractions ++ afterPassiveInteractions).map(updateBehaviour)
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
        random)
    }

    World.individuals.set(cells.flatten.toVector)(world)
  }


}

