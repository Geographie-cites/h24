package eighties.h24

import eighties.h24.generation.Interactions
import eighties.h24.opinion.InterchangeConviction
import eighties.h24.population._

import scala.util.Random

object TestOpinion extends App {

  def individual(opinion: Double, behaviour: Behaviour) =
    Individual(
      socialCategory = AggregatedSocialCategory.all.head,
      healthCategory = HealthCategory(opinion, behaviour, ChangeConstraints(true, true, false)),
      home = (0, 0),
      location = (0, 0),
      stableDestinations = Map.empty
    )


  val cell = Vector.fill(10)(individual(1.0, Healthy)) ++ Vector.fill(90)(individual(0.5, Unhealthy))

  val rng = new Random(42)

  def interact(cell: Vector[Individual]) =
    InterchangeConviction.interchangeConvictionInCell(
      cell,
      timeOfDay = 1,
      interactions = Map(AggregatedSocialCategory.all.head -> Interactions(1.0, 1.0, 1.0)),
      maxProbaToSwitch = 1.0,
      constraintsStrength = 0.0,
      inertiaCoefficient = 0.8,
      healthyDietReward = 1.0,
      rng
    )


  val lastCell = Iterator.iterate(cell)(interact).drop(99).next()

  println(cell.count(_.healthCategory.behaviour == Healthy))
  println(lastCell.count(_.healthCategory.behaviour == Healthy))

}
