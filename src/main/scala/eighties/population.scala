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
package eighties

import com.vividsolutions.jts.geom.Point
import eighties.generation.Feature
import space._
import monocle.macros.Lenses
import monocle.function.all._
import monocle.std.all._

object population {

  sealed trait Age {
    def from: Int
    def to: Int
  }

  object Age {
    case class AgeValue private(from: Int, to: Int) extends Age

    val From0To14 = AgeValue(0, 14)
    val From15To29 = AgeValue(15, 29)
    val From30To44 = AgeValue(30, 44)
    val From45To59 = AgeValue(45, 59)
    val From60To74 = AgeValue(60, 74)
    val Above75 = AgeValue(75, Int.MaxValue)

    def apply(code: Int) =
      code match {
        case 0 => Some(From0To14)
        case 1 => Some(From15To29)
        case 2 => Some(From30To44)
        case 3 => Some(From45To59)
        case 4 => Some(From60To74)
        case 5 => Some(Above75)
        case _ => None
      }
  }

  sealed trait Sex

  object Sex {
    object Male extends Sex
    object Female extends Sex

    def apply(code: Int) =
      code match {
        case 0 => Some(Male)
        case 1 => Some(Female)
        case _ => None
      }
  }

  sealed trait Education

  object Education {
    object Schol extends Education
    object Dipl0 extends Education
    object CEP extends Education
    object BEPC extends Education
    object CAPBEP extends Education
    object BAC extends Education
    object BACP2 extends Education
    object SUP extends Education

    def apply(code: Int) =
      code match {
        case 0 => Some(Schol)
        case 1 => Some(Dipl0)
        case 2 => Some(CEP)
        case 3 => Some(BEPC)
        case 4 => Some(CAPBEP)
        case 5 => Some(BAC)
        case 6 => Some(BACP2)
        case 7 => Some(SUP)
        case _ => None
      }
  }

  object Individual {
    def apply(feature: Feature): Option[Individual] =
      for {
        age <- Age(feature.age)
        sex <- Sex(feature.sex)
        education <- Education(feature.education)
        point = feature.point
      } yield
        Individual(
          age,
          sex,
          education,
          (point.getX, point.getY),
          space.project(point)
        )

    def i = Individual.location composeLens first
    def j = Individual.location composeLens second
    def x = Individual.location composeLens first
    def y = Individual.location composeLens second
  }

  @Lenses case class Individual(
    age: Age,
    sex: Sex,
    education: Education,
    home: Coordinate,
    location: Location)

}
