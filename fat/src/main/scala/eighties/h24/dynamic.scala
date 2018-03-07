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
import java.util.Calendar

import better.files._
import com.vividsolutions.jts.geom.Envelope
import com.vividsolutions.jts.index.strtree.STRtree
import eighties.h24.dynamic.MoveMatrix.{TimeSlice, TimeSlices}
import eighties.h24.generation._
import eighties.h24.population._
import eighties.h24.space._
import monocle.Monocle._
import org.joda.time.{Instant, Interval}

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

  def getTimeIndex(i: Instant, intervals: Array[Interval]) = intervals.indexWhere(p=>p.contains(i))

  def getTimeIndices(i: Interval, intervals: Array[Interval]) = intervals.zipWithIndex.filter(v => v._1.contains(i)).map(_._2)

  object MoveMatrix {

    object TimeSlice {
      def fromHours(from: Int, to: Int): TimeSlice = new TimeSlice(from * 60, to * 60)
    }

    case class TimeSlice(from: Int, to: Int) {
      def length = to - from
    }

    type TimeSlices = Array[(TimeSlice, CellMatrix)]
    type CellMatrix = Array[Array[Cell]]
    type Cell = Map[AggregatedSocialCategory, Array[Move]]
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

    /*
    def cell(location: Location) =
      index[Array[Array[Cell]], Int, Array[Cell]](location._1) composeOptional index(location._2)
*/
    def cells =
      each[TimeSlices, (TimeSlice, CellMatrix)] composeLens
        second[(TimeSlice, CellMatrix), CellMatrix] composeTraversal
        each[CellMatrix, Array[Cell]] composeTraversal
        each[Array[Cell], Cell]

    def allMoves =
      cells composeTraversal
        each[Cell, Array[Move]] composeTraversal each[Array[Move], Move]

    def moves(category: AggregatedSocialCategory => Boolean) =
      cells composeTraversal
        filterIndex[Cell, AggregatedSocialCategory, Array[Move]](category) composeTraversal
        each[Array[Move], Move]

//    def movesInNeighborhood(cellMatrix: CellMatrix, category: AggregatedSocialCategory, neighbor: Location => Boolean) =
//      for {
//        (line, i) <- cellMatrix.zipWithIndex
//        (cell, j) <- line.zipWithIndex
//        loc = Location(i,j)
//        if (neighbor(loc))
//        moves <- cell.get(category).toSeq
//      } yield (loc -> moves)

    def movesInNeighborhood(location: Location, category: AggregatedSocialCategory, index: STRtree) =
      for {
        (l,c) <- index.query(new Envelope(location._1 - 10, location._1 + 10, location._2 - 10, location._2 + 10)).toArray.toSeq.map(_.asInstanceOf[LCell])
        moves <- c.get(category)
      } yield (l -> moves)

    def movesInNeighborhoodByCategory(location: Location, index: STRtree) =
      for {
        (l,c) <- index.query(new Envelope(location._1 - 10, location._1 + 10, location._2 - 10, location._2 + 10)).toArray.toSeq.map(_.asInstanceOf[LCell])
      } yield (l -> c)
    //category: AggregatedSocialCategory

    def location = first[Move, Location]
    def moveRatio = second[Move, Double]

    def noMove(i: Int, j: Int) =
      Array.tabulate(i, j) {(ii, jj) => AggregatedSocialCategory.all.map { c => c -> Array((ii, jj) -> 1.0) }.toMap }

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
    val aggregatedCategory = Individual.socialCategory.get(individual)
    def myCategory = cellMoves.get(aggregatedCategory)
    def noSex = cellMoves.find { case(c, v) => c.age == aggregatedCategory.age && c.education == aggregatedCategory.education }.map(_._2)
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
  def noMoveInMoveMatrix(world: World, moves: MoveMatrix.CellMatrix, timeSlice: TimeSlice, random: Random) = {
    world
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
          cell applyTraversal (each[Array[Individual], Individual] composeLens Individual.opinion) modify { b: Double =>
            opinion.binomialAdoption(b, cellBehaviours, gama, random)
          }
        }
      }

    World.individuals.set(newIndividuals.flatten.toArray)(world)
  }

  def assignRandomDayLocation(world: World, timeSlices: MoveMatrix.TimeSlices, rng: Random) =
    World.allIndividuals.modify { individual =>
      val workTimeMoves = timeSlices.toMap.apply(dayTimeSlice)
      dynamic.sampleDestinationInMoveMatrix(workTimeMoves, individual, rng) match {
        case Some(d) => Individual.stableDestinations.modify(_ + (dayTimeSlice -> d))(individual)
        case None => Individual.stableDestinations.modify(_ + (dayTimeSlice -> individual.home))(individual)
      }
    }(world)

  def assignFixNightLocation(world: World, timeSlices: TimeSlices) =
    World.allIndividuals.modify { individual => Individual.stableDestinations.modify(_ + (nightTimeSlice -> individual.home))(individual) } (world)

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
    World.attractions.set(attractions.toArray)(world)
  }

  def logistic(l: Double, k: Double, x0: Double)(x: Double) = l / (1.0 +  math.exp(-k * (x - x0)))

  //def contact(teta: Double)(x: Double) = if(x < teta) 0.0 else 1.0

}
