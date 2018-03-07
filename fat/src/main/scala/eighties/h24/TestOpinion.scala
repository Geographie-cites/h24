package eighties.h24

import eighties.h24.generation.Interactions
import eighties.h24.opinion.InterchangeConviction
import eighties.h24.population._

import scala.util.Random

object OpinionFunction {

  def individual(opinion: Double, behaviour: Behaviour) =
    Individual(
      socialCategory = AggregatedSocialCategory.all.head,
      healthCategory = HealthCategory(opinion, behaviour, ChangeConstraints(true, true, false)),
      home = (0, 0),
      location = (0, 0),
      stableDestinations = Map.empty
    )

  def balanced = Vector.fill(50)(individual(0.9, Healthy)) ++ Vector.fill(50)(individual(0.1, Unhealthy))
  def unhealthyMajority = Vector.fill(10)(individual(0.9, Healthy)) ++ Vector.fill(90)(individual(0.1, Unhealthy))
  def healthyMajority = Vector.fill(90)(individual(0.9, Healthy)) ++ Vector.fill(10)(individual(0.1, Unhealthy))
  def balancedMiddle = Vector.fill(50)(individual(0.5, Healthy)) ++ Vector.fill(50)(individual(0.5, Unhealthy))


  def interact(cell: Vector[Individual], steps: Int, rng: Random) = {
    def step(cell: Vector[Individual]) = InterchangeConviction.interchangeConvictionInCell(
      cell,
      timeOfDay = 1,
      interactions = Map(AggregatedSocialCategory.all.head -> Interactions(1.0, 1.0, 1.0)),
      maxProbaToSwitch = 1.0,
      constraintsStrength = 0.0,
      inertiaCoefficient = 0.2,
      healthyDietReward = 1.0,
      interpersonalInfluence = 0.001,
      rng
    )

    Iterator.iterate(cell)(step).drop(steps).next()
  }


}

object TestOpinion extends App {
  import OpinionFunction._

  val rng = new Random(42)
  val cell = balanced
  val lastCell = interact(cell, 9999, rng)
  println(cell.count(_.healthCategory.behaviour == Healthy) + " " + cell.map(_.healthCategory.opinion).sum / 100)
  println(lastCell.count(_.healthCategory.behaviour == Healthy)+ " " + lastCell.map(_.healthCategory.opinion).sum / 100)
}
