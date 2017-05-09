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


import generation._
import space._
import monocle.function.all._
import monocle.macros.Lenses
import monocle.std.all._

import scala.util.Random

object population {

  sealed class Age(val from: Int, val to: Option[Int])

  object Age {

    object From0To14 extends Age(0, Some(14))
    object From15To29 extends Age(15, Some(29))
    object From30To44 extends Age(30, Some(44))
    object From45To59 extends Age(45, Some(59))
    object From60To74 extends Age(60, Some(74))
    object Above75 extends Age(75, None)

    def all = Vector(From0To14, From15To29, From30To44, From45To59, From60To74, Above75)

    def apply(code: Int): Option[Age] =
      code match {
        case 0 => Some(From0To14)
        case 1 => Some(From15To29)
        case 2 => Some(From30To44)
        case 3 => Some(From45To59)
        case 4 => Some(From60To74)
        case 5 => Some(Above75)
        case _ => None
      }

    def parse(age:Int) =
      if (age < 15) From0To14 else
      if (age < 30) From15To29 else
      if (age < 45) From30To44 else
      if (age < 60) From45To59 else
      if (age < 75) From60To74 else Above75
  }


  sealed trait AggregatedAge

  object AggregatedAge {
    object Junior extends AggregatedAge
    object Senior extends AggregatedAge
    object Veteran extends AggregatedAge

    def apply(age: Age) = age match {
      case Age.From0To14 | Age.From15To29 => Junior
      case Age.From30To44 | Age.From45To59 => Senior
      case Age.From60To74 | Age.Above75 => Veteran
    }

    def all = Vector(Junior, Senior, Veteran)
  }

  sealed trait Sex

  object Sex {
    object Male extends Sex
    object Female extends Sex

    def all = Vector(Male, Female)

    def apply(code: Int) =
      code match {
        case 0 => Some(Male)
        case 1 => Some(Female)
        case _ => None
      }
  }

  sealed trait Education {
    override def toString = getClass.getName
  }

  object Education {
    object Schol extends Education
    object Dipl0 extends Education
    object CEP extends Education
    object BEPC extends Education
    object CAPBEP extends Education
    object BAC extends Education
    object BACP2 extends Education
    object SUP extends Education

    def all = Vector(Schol, Dipl0, CEP, BEPC, CAPBEP, BAC, BACP2, SUP)

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

  sealed trait AggregatedEducation {
    override def toString = getClass.getName
  }

  object AggregatedEducation {
    object Low extends AggregatedEducation
    object Middle extends AggregatedEducation
    object High extends AggregatedEducation

    def all = Vector(Low, Middle, High)

    def apply(education: Education) =
      education match {
        case Education.Dipl0 | Education.BEPC | Education.CAPBEP | Education.CEP => Some(Low)
        case Education.BAC | Education.BACP2 => Some(Middle)
        case Education.SUP => Some(High)
        case _ => None
      }

  }

  type Behaviour = Double

  object Individual {
    def apply(feature: IndividualFeature, behaviour: (IndividualFeature, Random) => Behaviour, random: Random): Option[Individual] = {
      for {
        age <- Age(feature.ageCategory)
        sex <- Sex(feature.sex)
        education <- Education(feature.education)
      } yield
        Individual(
          age,
          sex,
          education,
          behaviour(feature, random),
          feature.location,
          feature.location
        )
    }


    def i = Individual.location composeLens first
    def j = Individual.location composeLens second
  }


  @Lenses case class Individual(
    age: Age,
    sex: Sex,
    education: Education,
    behaviour: Behaviour,
    home: Location,
    location: Location)


  object Category {
    def apply(individual: Individual): Category =
      Category(
        age = individual.age,
        sex = individual.sex,
        education = individual.education
      )

    def all =
      for {
        age <- Age.all
        sex <- Sex.all
        education <- Education.all
      } yield Category(age, sex, education)
  }

  case class Category(age: Age, sex: Sex, education: Education)

}
