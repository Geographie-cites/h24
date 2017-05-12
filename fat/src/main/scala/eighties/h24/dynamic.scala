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
package eighties.h24

import java.io.{FileInputStream, FileOutputStream}

import better.files._
import com.vividsolutions.jts.geom.Envelope
import com.vividsolutions.jts.index.strtree.STRtree
import eighties.h24.dynamic.MoveMatrix.{CellMatrix, TimeSlice, TimeSlices}
import eighties.h24.generation.{Interactions, workTimeSlice}
import eighties.h24.population._
import eighties.h24.space._
import monocle.Monocle._
import org.joda.time.{DateTime, Instant, Interval}
import squants._

import scala.util.Random

object dynamic {

  type Move = (World, Random) => Individual => Individual

  def reachable[T](index: Index[T]) =
    for {
      i <- 0 until index.sideI
      j <- 0 until index.sideJ
      if !index.cells(i)(j).isEmpty
    } yield (i, j)

  def randomMove(world: World, ratio: Double, random: Random) = {
    val reach = reachable(Index.indexIndividuals(world))
    val rSize = reach.size
    def randomLocation =
      Individual.location.modify(l => if(random.nextDouble() < ratio) reach(random.nextInt(rSize)) else l)

    (World.allIndividuals modify randomLocation) (world)
  }

  def goBackHome(world: World) = {
    def m = (individual: Individual) => Individual.location.set(individual.home)(individual)
    (World.allIndividuals modify m)(world)
  }

  def getTimeIndex(i: Instant, intervals: Vector[Interval]) = intervals.indexWhere(p=>p.contains(i))

  def getTimeIndices(i: Interval, intervals: Vector[Interval]) = intervals.zipWithIndex.filter(v => v._1.contains(i)).map(_._2)

  object MoveMatrix {

    object TimeSlice {
      def fromHours(from: Int, to: Int): TimeSlice = new TimeSlice(from * 60, to * 60)
    }

    case class TimeSlice(from: Int, to: Int) {
      def length = to - from
    }

    type TimeSlices = Vector[(TimeSlice, CellMatrix)]
    type CellMatrix = Vector[Vector[Cell]]
    type Cell = Map[AggregatedSocialCategory, Vector[Move]]
    type Move = (Location, Double)

    def getLocatedCells(timeSlice: TimeSlices) =
      for {
        (ts, matrix) <- timeSlice
        (line, i) <- matrix.zipWithIndex
        (c, j) <- line.zipWithIndex
      } yield (ts, (i, j), c)

    def getLocatedCellsFromCellMatrix(matrix: CellMatrix) =
      for {
        (line, i) <- matrix.zipWithIndex
        (c, j) <- line.zipWithIndex
      } yield ((i, j), c)

    def modifyCellMatrix(f: (Cell, Location) => Cell)(matrix: CellMatrix): CellMatrix =
      matrix.zipWithIndex.map { case(line, i) => line.zipWithIndex.map { case(c, j) => f(c, (i, j)) } }

    def cell(location: Location) =
      index[Vector[Vector[Cell]], Int, Vector[Cell]](location._1) composeOptional index(location._2)

    def cells =
      each[TimeSlices, (TimeSlice, CellMatrix)] composeLens
        second[(TimeSlice, CellMatrix), CellMatrix] composeTraversal
        each[CellMatrix, Vector[Cell]] composeTraversal
        each[Vector[Cell], Cell]

    def allMoves =
      cells composeTraversal
        each[Cell, Vector[Move]] composeTraversal each[Vector[Move], Move]

    def moves(category: AggregatedSocialCategory => Boolean) =
      cells composeTraversal
        filterIndex[Cell, AggregatedSocialCategory, Vector[Move]](category) composeTraversal
        each[Vector[Move], Move]

//    def movesInNeighborhood(cellMatrix: CellMatrix, category: AggregatedSocialCategory, neighbor: Location => Boolean) =
//      for {
//        (line, i) <- cellMatrix.zipWithIndex
//        (cell, j) <- line.zipWithIndex
//        loc = Location(i,j)
//        if (neighbor(loc))
//        moves <- cell.get(category).toSeq
//      } yield (loc -> moves)

    type LCell = ((Int, Int), Cell)
    def movesInNeighborhood(location: Location, category: AggregatedSocialCategory, index: STRtree) =
      for {
        (l,c) <- index.query(new Envelope(location._1 - 10, location._1 + 10, location._2 - 10, location._2 + 10)).toArray.toSeq.map(_.asInstanceOf[LCell])
        moves <- c.get(category)
      } yield (l -> moves)

    def location = first[Move, Location]
    def moveRatio = second[Move, Double]

    def noMove(i: Int, j: Int) =
      Vector.tabulate(i, j) {(ii, jj) => AggregatedSocialCategory.all.map { c => c -> Vector((ii, jj) -> 1.0) }.toMap }

    import boopickle.Default._

    implicit val categoryPickler = transformPickler((i: Int) => SocialCategory.all(i))(s => SocialCategory.all.indexOf(s))
    implicit val aggregatedCategoryPickler = transformPickler((i: Int) => AggregatedSocialCategory.all(i))(s => AggregatedSocialCategory.all.indexOf(s))

    def save(moves: TimeSlices, file: File) = {
      val os = new FileOutputStream(file.toJava)
      try os.getChannel.write(Pickle.intoBytes(moves))
      finally os.close()
    }

    def load(file: File) = {
      val is = new FileInputStream(file.toJava)
      try Unpickle[TimeSlices].fromBytes(is.getChannel.toMappedByteBuffer)
      finally is.close()
    }

  }

  def moveFlowDefaultOnOtherSex(moves: MoveMatrix.CellMatrix, individual: Individual) = {
    val location = Individual.location.get(individual)
    val cellMoves = moves(location._1)(location._2)
    val aggregatedCategory = AggregatedSocialCategory(Individual.socialCategory.get(individual))
    def myCategory = cellMoves.get(aggregatedCategory)
    def noSex = cellMoves.find { case(c, v) => c.age == aggregatedCategory.age && c.education == aggregatedCategory.education}.map(_._2)
    myCategory orElse noSex
  }

  def sampleDestinationInMoveMatrix(moves: MoveMatrix.CellMatrix, individual: Individual, random: Random) =
    moveFlowDefaultOnOtherSex(moves, individual).flatMap { m => if(m.isEmpty) None else Some(multinomial(m)(random)) }

  def moveInMoveMatrix(world: World, moves: MoveMatrix.CellMatrix, timeSlice: TimeSlice, random: Random) = {
    def sampleMoveInMatrix(individual: Individual) =
      individual.stableDestinations.get(timeSlice) match {
        case None =>
          sampleDestinationInMoveMatrix(moves, individual, random)  match {
            case None => individual
            case Some(destination) => Individual.location.set(destination)(individual)
          }
        case Some(destination) => Individual.location.set(destination)(individual)
    }
    (World.allIndividuals modify sampleMoveInMatrix)(world)
  }

  def localConviction(gama: Double, world: World, random: Random) = {
    def cs = Index.allCells[Individual].getAll(Index.indexIndividuals(world))

    def newIndividuals =
      cs.map { cell =>
        val size = cell.size
        if (size == 0) cell
        else {
          //val cellBehaviours = random.shuffle(cell.map(_.behaviour)).take((size * 0.01).toInt + 1).toArray
          val cellBehaviours = cell.map(i => Individual.opinion.get(i)).toArray
          cell applyTraversal (each[Vector[Individual], Individual] composeLens Individual.opinion) modify { b: Double =>
            opinion.binomialAdoption(b, cellBehaviours, gama, random)
          }
        }
      }

    World.individuals.set(newIndividuals.flatten.toVector)(world)
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

    def booleanToDouble(b: Boolean) = if(b) 1.0 else 0.0

    def interactionProbability(individual: Individual) = timeOfDay match {
      case 0 => interactions(AggregatedSocialCategory(individual.socialCategory)).breakfastInteraction
      case 1 => interactions(AggregatedSocialCategory(individual.socialCategory)).lunchInteraction
      case 2 => interactions(AggregatedSocialCategory(individual.socialCategory)).dinnerInteraction
    }

    def peering(cell: Vector[Individual]): (Vector[(Individual, Individual)], Vector[Individual]) = {
      val (interactingPeople, passivePeople) = cell.partition { individual => random.nextDouble() < interactionProbability(individual) }

      if(interactingPeople.size % 2 == 0) (random.shuffle(interactingPeople).grouped(2).toVector.map { case Vector(i1, i2) => (i1, i2) }, passivePeople)
      else (random.shuffle(interactingPeople).dropRight(1).grouped(2).toVector.map { case Vector(i1, i2) => (i1, i2) }, passivePeople ++ Seq(interactingPeople.last))
    }

    def dietReward(individual: Individual) = {
      def getReward(o: Opinion): Opinion =  math.min(1.0, (1 + healthyDietReward) * o)
      if(individual.healthCategory.behaviour == Healthy) (Individual.healthCategory composeLens HealthCategory.opinion modify getReward) (individual)
      else individual
    }

    def opinionInertia(previousOpinion: Opinion, opinion: Opinion): Opinion =
      previousOpinion * inertiaCoefficient + (1 - inertiaCoefficient) * opinion


    def updateInteractingOpinion(ego: Individual, partner: Individual, healthRatio: Option[Double]): Individual = {
      val influencePartner = (booleanToDouble(partner.healthCategory.behaviour == Healthy) + partner.healthCategory.opinion) / 2
      Individual.opinion.modify { o =>
        healthRatio match {
          case Some(hr) => opinionInertia(o, 0.5 * o + 0.25 * influencePartner + 0.25 * hr)
          case None => o // This wont happend
        }
      }(ego)
    }

    def updatePassiveOpinion(individual: Individual, healthRatio: Option[Double]): Individual = Individual.opinion.modify { o =>
      healthRatio match {
        case Some(hr) => opinionInertia(o, 0.75 * o + 0.25 * hr)
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

        math.max(0.0, y * (-2 * individual.healthCategory.opinion + 1))
      }

      def probaSwitchToHealthy = {
        val y =
          maxProbaToSwitch -
            booleanToDouble(individual.healthCategory.changeConstraints.budget) * constraintsStrength -
            booleanToDouble(individual.healthCategory.changeConstraints.time) * constraintsStrength -
            booleanToDouble(individual.healthCategory.changeConstraints.habit) * constraintsStrength

        math.max(0.0, y * (2 * individual.healthCategory.opinion - 1))
      }

      Individual.behaviour.modify {
        case Healthy => if (random.nextDouble() < probaSwitchToUnhealthy) Unhealthy else Healthy
        case Unhealthy => if (random.nextDouble() < probaSwitchToHealthy) Healthy else Unhealthy
      }(individual)
    }

    def cells = Index.allCells[Individual].getAll(Index.indexIndividuals(world)).map {cell =>
      val healthyRatio = if(!cell.isEmpty) Some(cell.count(_.healthCategory.behaviour == Healthy).toDouble / cell.size) else None
      val rewarded = cell.map(dietReward)
      val (interactingPeople, passivePeople) = peering(rewarded)

      val sens1 = interactingPeople.map { case(ego, partner) => updateInteractingOpinion(ego, partner, healthyRatio) }
      val sens2 = interactingPeople.map { case(partner, ego) => updateInteractingOpinion(ego, partner, healthyRatio)  }

      val afterInteractions = sens1 ++ sens2

      val afterPassiveInteractions = passivePeople.map(i => updatePassiveOpinion(i, healthyRatio))

      (afterInteractions ++ afterPassiveInteractions).map(updateBehaviour)
    }

    World.individuals.set(cells.flatten.toVector)(world)
  }


  def fixWorkPlace(world: World, timeSlices: TimeSlices, rng: Random) =
    World.allIndividuals.modify { individual =>
      val workTimeMoves = timeSlices.toMap.apply(workTimeSlice)
      dynamic.sampleDestinationInMoveMatrix(workTimeMoves, individual, rng) match {
        case Some(d) => Individual.stableDestinations.modify(_ + (workTimeSlice -> d))(individual)
        case None => Individual.stableDestinations.modify(_ + (workTimeSlice -> individual.home))(individual)
      }
    }(world)

  def randomiseLocation(world: World, random: Random) = {
    val reach = reachable(Index[Individual](world.individuals.iterator, Individual.location.get(_), world.sideI, world.sideJ))
    val reachSize = reach.size

    def assign(individual: Individual): Individual =
      Individual.home.set(reach(random.nextInt(reachSize))) (individual)

    (World.allIndividuals modify assign)(world)
  }

  def generateAttractions(world: World, proportion: Double, random: Random) = {
    val reach = reachable(Index.indexIndividuals(world))

    def attraction = {
      val location = reach.randomElement(random)
      val education = AggregatedEducation.all.randomElement(random)
      Attraction(location, education)
    }

    def attractions = (0 until (reach.size * proportion).toInt).map(_ => attraction)
    World.attractions.set(attractions.toVector)(world)
  }

  def logistic(l: Double, k: Double, x0: Double)(x: Double) = l / (1.0 +  math.exp(-k * (x - x0)))

  //def contact(teta: Double)(x: Double) = if(x < teta) 0.0 else 1.0

}
