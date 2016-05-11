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

import java.io.{BufferedInputStream, FileInputStream, InputStream}

import com.github.tototoshi.csv.CSVReader
import com.vividsolutions.jts.geom.{Coordinate, _}
import org.apache.commons.compress.compressors.lzma.LZMACompressorInputStream
import org.geotools.data.{DataUtilities, Transaction}
import org.geotools.data.shapefile.{ShapefileDataStore, ShapefileDataStoreFactory}
import org.geotools.geometry.jts.JTS
import org.geotools.referencing.CRS

import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import scala.util.{Random, Try}
import better.files._
import com.vividsolutions.jts.triangulate.ConformingDelaunayTriangulationBuilder
import eighties.population.Age
import eighties.population.Age.AgeValue

import scala.collection.mutable.ArrayBuffer
import scala.util.Random
import scalaz._
import Scalaz._


object generation {
  object SchoolAge {
    val From0To1 = AgeValue(0, Some(1))
    val From2To5 = AgeValue(2, Some(5))
    val From6To10 = AgeValue(6, Some(10))
    val From11To14 = AgeValue(11, Some(14))
    val From15To17 = AgeValue(15, Some(17))
    val From18To24 = AgeValue(18, Some(24))
    val From25To29 = AgeValue(25, Some(29))
    val Above30 = AgeValue(30, None)

    def all = Vector(From0To1, From2To5, From6To10, From11To14, From15To17, From18To24, From25To29, Above30)
    def index(age: Double) = all.indexWhere(value => age > value.from)
  }

  type IrisID = String
  case class Feature(ageCategory: Int, age: Option[Double], sex: Int, education: Int, point: Point)

  def toDouble(s: String) =
    s.filter(_ != '"').replace(',', '.') match {
      case "" => 0.0
      case x => x.toDouble
    }

  def readGeometry(aFile: File) = {
    val store = new ShapefileDataStore(aFile.toJava.toURI.toURL)
    val reader = store.getFeatureReader
    val featureReader = Iterator.continually(reader.next).takeWhile(_ => reader.hasNext)
    val result =
      Try {
        featureReader
          .filter(feature =>feature.getAttribute("DCOMIRIS").toString.startsWith("75"))
          .map { feature =>
            val geom = feature.getDefaultGeometry.asInstanceOf[MultiPolygon].getGeometryN(0).asInstanceOf[Polygon]
            val iris = feature.getAttribute("DCOMIRIS").toString
            iris -> geom
          }.toMap
      }
    reader.close
    store.dispose

    result
  }

  def readEducationSex(stream: InputStream) = {
    val reader = CSVReader.open(Source.fromInputStream(stream))
    val result =
      Try {
        reader.iterator.map { line =>
          val men = line.drop(36).take(7).map(toDouble).toVector
          val women = line.drop(44).take(7).map(toDouble).toVector
          line(0) -> Vector(men, women)
        }.toMap
      }
    reader.close
    result
  }

  def readAgeSchool(stream: InputStream) = {
    val reader = CSVReader.open(Source.fromInputStream(stream))
    val result =
      Try {
        reader.iterator.map { line =>
          val totalPop = line.drop(13).take(7).map(toDouble).toVector
          val schooled = line.drop(20).take(7).map(toDouble).toVector
          line(0) -> ((schooled zip totalPop).map { case (x,y)=>y/x })
        }.toMap
      }
    reader.close
    result
  }

  def readAgeSex(stream: InputStream) = {
    val reader = CSVReader.open(Source.fromInputStream(stream))
    val result =
      Try {
        reader.iterator.map { line =>
          val men = line.drop(34).take(6).map(toDouble).toVector
          val women = line.drop(44).take(6).map(toDouble).toVector
          line(0) -> (men ++ women)
        }.toMap
      }
    reader.close
    result
  }

  def generatePopulation(rnd: Random, geometry: Map[IrisID, Polygon], ageSex: Map[IrisID, Vector[Double]],
                         schoolAge: Map[IrisID, Vector[Double]], educationSex: Map[IrisID, Vector[Vector[Double]]]) = {
    geometry.toSeq.map {
      case (id, geom) => {
        val ageSexV = ageSex.get(id).get
        val schoolAgeV = schoolAge.get(id).get
        val educationSexV = educationSex.get(id).get
        val sampler = new PolygonSampler(geom)

        val total = ageSexV.sum

        val ageSexSizes = Seq(6,2)
        val ageSexVariate = new RasterVariate(ageSexV, ageSexSizes)

        val educationSexSizes = Seq(7)
        val educationSexVariates = ArrayBuffer(
          new RasterVariate(educationSexV(0), educationSexSizes),
          new RasterVariate(educationSexV(1), educationSexSizes))

        def rescale(min: Double, max: Double, value: Double) = min + value * (max - min)
        val res = (0 to total.toInt).map{_=>
          val sample = ageSexVariate.compute(rnd)
          val ageIndex = (sample(0)*ageSexSizes(0)).toInt
          val ageInterval = Age.all(ageIndex)
          val residual = sample(0)*ageSexSizes(0) - ageIndex
          val age = ageInterval.to.map(max => rescale(ageInterval.from, max, residual))
          val sex = (sample(1)*ageSexSizes(1)).toInt

          val schooled = age match {
            case Some(a) =>
              val schoolAgeIndex = SchoolAge.index(a)
              if (schoolAgeIndex == 0) false else {
                val proba = schoolAgeV(schoolAgeIndex - 1)
                (rnd.nextDouble() < proba)
              }
            case None => false
          }
          val education = if (schooled) 0 else {
            if (ageIndex > 0) (educationSexVariates(sex).compute(rnd)(0) * educationSexSizes(0)).toInt + 1
            else 1
          }
          Feature(
            ageCategory = ageIndex,
            age = age,
            sex = sex,
            education = education,
            point = geom.getFactory.createPoint(sampler.apply(rnd))
          )
        }
        res
      }
    }
  }

  def generateFeatures(inputDirectory: File, rng: Random) = {
    val contourIRISFile = inputDirectory / "CONTOURS-IRIS_FE_IDF.shp"
    val baseICEvolStructPopFileName = inputDirectory / "base-ic-evol-struct-pop-2012-IDF.csv.lzma"
    val baseICDiplomesFormationPopFileName = inputDirectory / "base-ic-diplomes-formation-2012-IDF.csv.lzma"
    val outFileName = inputDirectory / "generated-population-75.shp"
    val specs = "geomLAEA:Point:srid=3035,cellX:Integer,cellY:Integer,age:Integer,sex:Integer,education:Integer"
    val factory = new ShapefileDataStoreFactory
    val inBaseICEvolStructPop = new BufferedInputStream(new FileInputStream(baseICEvolStructPopFileName.toJava))
    val inBaseICDiplomesFormationPop = new BufferedInputStream(new FileInputStream(baseICDiplomesFormationPopFileName.toJava))
    for {
      geom <- readGeometry(contourIRISFile)
      ageSex <- readAgeSex(new LZMACompressorInputStream(inBaseICEvolStructPop))
      schoolAge <- readAgeSchool(new LZMACompressorInputStream(inBaseICDiplomesFormationPop))
      educationSex <- readEducationSex(new LZMACompressorInputStream(inBaseICDiplomesFormationPop))
    } yield generatePopulation(rng, geom, ageSex, schoolAge, educationSex).toIterator.flatten
  }


  class RasterVariate(pdf: Seq[Double], val m_size: Seq[Int]) {
    val N = m_size.size
    val m_totsize = m_size.product
    val m_cdf = buildCdf(m_totsize, pdf, m_size).toParArray
    val m_sum = pdf.foldLeft(0.0)((a, b) => a + b)

    def buildCdf(totsize: Int, pdf: Seq[Double], size: Seq[Int]) = {
      var sum = 0.0
      var cdf = ArrayBuffer(0.0)
      for (i <- 0 until totsize) {
        sum = sum + pdf(i)
        cdf.append(sum)
      }
      cdf = cdf.map(_ / sum)
      cdf.toIndexedSeq
    }

    def compute(rng: Random): Vector[Double] = {
      def dim(i: Int) = State[(Int, Random), Double] { case(offset, rng) =>
        val ix = offset % m_size(i)
        val newOffset = offset / m_size(i)
        val v = (ix + rng.nextDouble()) / m_size(i)
        ((newOffset, rng), v)
      }

      val x = rng.nextDouble()
      val offset = m_cdf.indexWhere(p => p > x) - 1

      (0 until N).toVector.map(dim).sequenceU.eval((offset, rng))
    }
  }

  class PolygonSampler(val polygon: Polygon, val tolerance: Double = 0.1) {
    val triangles = {
      val builder = new ConformingDelaunayTriangulationBuilder
      builder.setSites(polygon)
      builder.setConstraints(polygon)
      builder.setTolerance(tolerance)
      val triangleCollection = builder.getTriangles(polygon.getFactory()).asInstanceOf[GeometryCollection]
      var areaSum = 0.0
      val trianglesInPolygon = (0 until triangleCollection.getNumGeometries).map(triangleCollection.getGeometryN(_).asInstanceOf[Polygon]).filter(p => {
        val area = p.getArea
        p.intersection(polygon).getArea() > 0.99 * area
      })
      trianglesInPolygon.map { triangle =>
        areaSum += triangle.getArea
        (areaSum, triangle)
      }
    }
    val totalArea = triangles.last._1
    def apply(rnd: Random) = {
      val s = rnd.nextDouble() * totalArea
      val t = rnd.nextDouble()
      val triangleIndex = triangles.indexWhere(s < _._1)
      val area = triangles(triangleIndex)._1
      val previousArea = if (triangles.isDefinedAt(triangleIndex - 1)) triangles(triangleIndex - 1)._1 else 0.0
      val triangle = triangles(triangleIndex)._2
      val tmp = Math.sqrt((s - previousArea) / (area - previousArea))
      val a = 1 - tmp
      val b = (1 - t) * tmp
      val c = t * tmp
      val coord = triangle.getCoordinates
      val p1 = coord(0)
      val p2 = coord(1)
      val p3 = coord(2)
      val x1 = p1.x
      val x2 = p2.x
      val x3 = p3.x
      val y1 = p1.y
      val y2 = p2.y
      val y3 = p3.y
      val x = a * x1 + b * x2 + c * x3
      val y = a * y1 + b * y2 + c * y3
      new Coordinate(x,y)
    }
  }

}
