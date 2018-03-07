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


import eighties.h24.dynamic.MoveMatrix.TimeSlice
import generation._
import space._
import monocle.function.all._
import monocle.macros.Lenses
import monocle.std.all._

import scala.util.Random

object population {

  /* -------------------------- Social Category -------------------------- */

  sealed class Age(val from: Int, val to: Option[Int])

  object Age {

    object From0To14 extends Age(0, Some(14))
    object From15To29 extends Age(15, Some(29))
    object From30To44 extends Age(30, Some(44))
    object From45To59 extends Age(45, Some(59))
    object From60To74 extends Age(60, Some(74))
    object Above75 extends Age(75, None)

    def all = Array(From0To14, From15To29, From30To44, From45To59, From60To74, Above75)

    def apply(code: Int): Age =
      code match {
        case 0 => From0To14
        case 1 => From15To29
        case 2 => From30To44
        case 3 => From45To59
        case 4 => From60To74
        case 5 => Above75
      }

    def parse(age: Int) =
      if (age < 15) From0To14 else
      if (age < 30) From15To29 else
      if (age < 45) From30To44 else
      if (age < 60) From45To59 else
      if (age < 75) From60To74 else Above75
  }


  sealed trait AggregatedAge

  object AggregatedAge {
    object Junior extends AggregatedAge {
      override def toString = "Junior"
    }

    object Senior extends AggregatedAge{
      override def toString = "Senior"
    }

    object Veteran extends AggregatedAge {
      override def toString = "Veteran"
    }

    def apply(age: Age) = age match {
      case Age.From0To14 | Age.From15To29 => Junior
      case Age.From30To44 | Age.From45To59 => Senior
      case Age.From60To74 | Age.Above75 => Veteran
    }

    def all = Array(Junior, Senior, Veteran)

    def toCode(age: AggregatedAge) =
      age match {
        case Junior => "1"
        case Senior => "2"
        case Veteran => "3"
      }
  }

  sealed trait Sex

  object Sex {
    object Male extends Sex {
      override def toString = "Male"
    }
    object Female extends Sex{
      override def toString = "Female"
    }

    def all = Array(Male, Female)

    def apply(code: Int): Sex =
      code match {
        case 0 => Male
        case 1 => Female
      }

    def toCode(sex: Sex) =
      sex match {
        case Male => "1"
        case Female => "2"
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

    def all = Array(Schol, Dipl0, CEP, BEPC, CAPBEP, BAC, BACP2, SUP)

    def apply(code: Int): Education =
      code match {
        case 0 => Schol
        case 1 => Dipl0
        case 2 => CEP
        case 3 => BEPC
        case 4 => CAPBEP
        case 5 => BAC
        case 6 => BACP2
        case 7 => SUP
      }
  }

  sealed trait AggregatedEducation

  object AggregatedEducation {
    object Low extends AggregatedEducation {
      override def toString = "Low"
    }
    object Middle extends AggregatedEducation {
      override def toString = "Middle"
    }
    object High extends AggregatedEducation{
      override def toString = "High"
    }

    def all = Array(Low, Middle, High)

    def apply(education: Education) =
      education match {
        case Education.Schol | Education.Dipl0 | Education.BEPC | Education.CAPBEP | Education.CEP => Low
        case Education.BAC | Education.BACP2 => Middle
        case Education.SUP => High
      }

    def toCode(education: AggregatedEducation) =
      education match {
        case Low => "1"
        case Middle => "2"
        case High => "3"
      }

  }


  object SocialCategory {
    def all =
      for {
        age <- Age.all
        sex <- Sex.all
        education <- Education.all
      } yield SocialCategory(age, sex, education)

    def apply(feature: IndividualFeature): SocialCategory =
      new SocialCategory(
        sex = Sex(feature.sex),
        age = Age(feature.ageCategory),
        education = Education(feature.education))

  }

  @Lenses case class SocialCategory(age: Age, sex: Sex, education: Education)


  object AggregatedSocialCategory {
    def apply(category: SocialCategory): AggregatedSocialCategory =
      new AggregatedSocialCategory(
        age = AggregatedAge(category.age),
        sex = category.sex,
        education = AggregatedEducation(category.education)
      )

    def apply(feature: IndividualFeature): AggregatedSocialCategory =
      AggregatedSocialCategory(SocialCategory(feature))

    def all =
      for {
        sex <- Sex.all
        age <- AggregatedAge.all
        education <- AggregatedEducation.all
      } yield AggregatedSocialCategory(age, sex, education)

    def toCode(aggregatedSocialCategory: AggregatedSocialCategory) =
      Array(
        Sex.toCode(aggregatedSocialCategory.sex),
        AggregatedAge.toCode(aggregatedSocialCategory.age),
        AggregatedEducation.toCode(aggregatedSocialCategory.education))
  }

  @Lenses case class AggregatedSocialCategory(
    age: AggregatedAge,
    sex: Sex,
    education: AggregatedEducation)


  /* --------------------------- Heath Category -------------------------- */
  type Opinion = Double

  sealed trait Behaviour
  object Healthy extends Behaviour
  object Unhealthy extends Behaviour

  @Lenses case class ChangeConstraints(habit: Boolean, budget: Boolean, time: Boolean)

  @Lenses case class HealthCategory(
    opinion: Opinion,
    behaviour: Behaviour,
    changeConstraints: ChangeConstraints)

  /* ---------------------------- Individual ------------------------------ */


  object Individual {
    def apply(
      feature: IndividualFeature,
      healthCategory: (AggregatedSocialCategory, Random) => HealthCategory,
      random: Random,
      stableDestinations: Map[TimeSlice, Location] = Map.empty): Individual = {
      val socialCategory = AggregatedSocialCategory(feature)

      Individual(
        socialCategory = socialCategory,
        healthCategory = healthCategory(socialCategory, random),
        feature.location,
        feature.location,
        stableDestinations
      )
    }

    def behaviour = Individual.healthCategory composeLens HealthCategory.behaviour
    def opinion = Individual.healthCategory composeLens HealthCategory.opinion
    def education = Individual.socialCategory composeLens AggregatedSocialCategory.education
    def age = Individual.socialCategory composeLens AggregatedSocialCategory.age
    def sex = Individual.socialCategory composeLens AggregatedSocialCategory.sex
    def i = Individual.location composeLens first
    def j = Individual.location composeLens second
  }

  @Lenses case class Individual(
    socialCategory: AggregatedSocialCategory,
    healthCategory: HealthCategory,
    home: Location,
    location: Location,
    stableDestinations: Map[TimeSlice, Location])
}
