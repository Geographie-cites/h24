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
import org.geotools.geometry.jts.{JTS, JTSFactoryFinder}
import org.geotools.referencing.CRS

import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import scala.util.{Random, Try}
import better.files._
import com.vividsolutions.jts.triangulate.ConformingDelaunayTriangulationBuilder
import eighties.population.Age
import eighties.population.Age.AgeValue
import org.apache.commons.math3.distribution.PoissonDistribution
import org.apache.commons.math3.random.RandomGenerator

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
    def index(age: Double) = SchoolAge.all.lastIndexWhere(value => age > value.from)
  }

  type IrisID = String
  case class Feature(ageCategory: Int, age: Option[Double], sex: Int, education: Int, point: Point, location: space.Coordinate)
  case class Equipment(typeEquipment: String, point: Point, location:space.Coordinate, quality:String, iris:String)
  case class Activity(point: Point, location: space.Coordinate)

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
            val geom = feature.getDefaultGeometry.asInstanceOf[MultiPolygon]
            val iris = feature.getAttribute("DCOMIRIS").toString
            iris -> geom
          }.toMap
      }
    reader.close
    store.dispose

    result
  }

  def withCSVReader[T](file: File)(f: CSVReader => T) = {
    val in = new BufferedInputStream(new FileInputStream(file.toJava))
    val stream = new LZMACompressorInputStream(in)
    val reader = CSVReader.open(Source.fromInputStream(stream,"ISO-8859-1"))
    try f(reader)
    finally reader.close
  }

  def readEducationSex(file: File) = withCSVReader(file){ reader =>
    Try {
      reader.iterator.drop(6).map { line =>
        val men = line.drop(36).take(7).map(toDouble).toVector
        val women = line.drop(44).take(7).map(toDouble).toVector
        line(0) -> Vector(men, women)
      }.toMap
    }
  }

  def readAgeSchool(file: File) = withCSVReader(file){ reader =>
    Try {
      reader.iterator.drop(6).map { line =>
        val totalPop = line.drop(13).take(7).map(toDouble).toVector
        val schooled = line.drop(20).take(7).map(toDouble).toVector
        line(0) -> ((schooled zip totalPop).map { case (x,y)=> x / y })
      }.toMap
    }
  }

  def readAgeSex(file: File) = withCSVReader(file){ reader =>
    Try {
      reader.iterator.drop(6).map { line =>
        val men = line.drop(34).take(6).map(toDouble).toVector
        val women = line.drop(44).take(6).map(toDouble).toVector
        line(0) -> (men ++ women)
      }.toMap
    }
  }

  def readEquipment(file: File) = withCSVReader(file){ reader =>
    Try {
      reader.iterator.drop(1).map { line =>
        val iris = line(4).trim.replaceAll("_","").replaceAll("0000$","")
        val typeEquipment = line(5)
        val x = line(6)
        val y = line(7)
        val quality = line(8)
        iris -> Vector(typeEquipment,x,y,quality)
      }.toMap
    }
  }

  def generatePopulation(rnd: Random, geometry: Map[IrisID, MultiPolygon], ageSex: Map[IrisID, Vector[Double]],
                         schoolAge: Map[IrisID, Vector[Double]], educationSex: Map[IrisID, Vector[Vector[Double]]]) = {
    val inCRS = CRS.decode("EPSG:2154")
    val outCRS = CRS.decode("EPSG:3035")
    val transform = CRS.findMathTransform(inCRS, outCRS, true)

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
        val res = (0 until total.toInt).map{_=>
          val sample = ageSexVariate.compute(rnd)
          val ageIndex = (sample(0)*ageSexSizes(0)).toInt
          val ageInterval = Age.all(ageIndex)
          val residual = sample(0)*ageSexSizes(0) - ageIndex
          val age = ageInterval.to.map(max => rescale(ageInterval.from, max, residual))
          val sex = (sample(1)*ageSexSizes(1)).toInt

          var tempIndex = -1
          var tempP = -1.0
          val schooled = age match {
            case Some(a) =>
              val schoolAgeIndex = SchoolAge.index(a)
              tempIndex = schoolAgeIndex
              if (schoolAgeIndex == 0) false else {
                tempP = schoolAgeV(schoolAgeIndex - 1)
                rnd.nextDouble() < schoolAgeV(schoolAgeIndex - 1)
              }
            case None => false
          }
          val education = if (schooled) 0 else {
            if (ageIndex > 0) (educationSexVariates(sex).compute(rnd)(0) * educationSexSizes(0)).toInt + 1
            else 1
          }
          val coordinate = sampler.apply(rnd)
          val transformed = JTS.transform(coordinate, null, transform)
          val point = JTS.toGeometry(JTS.toDirectPosition(transformed, outCRS))
          Feature(
            ageCategory = ageIndex,
            age = age,
            sex = sex,
            education = education,
            point = point,
            location = (point.getX,point.getY)
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

    for {
      geom <- readGeometry(contourIRISFile)
      ageSex <- readAgeSex(baseICEvolStructPopFileName)
      schoolAge <- readAgeSchool(baseICDiplomesFormationPopFileName)
      educationSex <- readEducationSex(baseICDiplomesFormationPopFileName)
    } yield generatePopulation(rng, geom, ageSex, schoolAge, educationSex).toIterator.flatten
  }

  def generateEquipment(rnd: Random, eq: Map[IrisID, Vector[String]], geometry: Map[IrisID, MultiPolygon]) = {
    val l93CRS = CRS.decode("EPSG:2154")
    val l2eCRS = CRS.decode("EPSG:27572")
    val outCRS = CRS.decode("EPSG:3035")
    val transformL93 = CRS.findMathTransform(l93CRS, outCRS, true)
    val transformL2E = CRS.findMathTransform(l2eCRS, outCRS, true)

    eq.toSeq.map {
      case (id, vec) => {
        val typeEquipment = vec(0)
        val x = vec(1)
        val y = vec(2)
        val quality = vec(3).trim
        if (quality.equalsIgnoreCase("bonne") || quality.equalsIgnoreCase("acceptable")) {
          val coordinate = new Coordinate(x.toDouble, y.toDouble)
          val transformed = JTS.transform(coordinate, null, transformL2E)
          val point = JTS.toGeometry(JTS.toDirectPosition(transformed, outCRS))
          Equipment(
            typeEquipment = typeEquipment,
            point = point,
            location = (point.getX, point.getY),
            quality = quality,
            iris = id
          )
        } else {
          val irises = geometry.filter{case (key,g)=>key.startsWith(id)}
          val size = irises.size
          if (size == 0) {
            println(s"Could Not find IRIS $id")
            Equipment(
              typeEquipment = typeEquipment,
              point = JTS.toGeometry(JTS.toDirectPosition(new Coordinate(0.0,0.0), outCRS)),
              location = (0.0, 0.0),
              quality = quality,
              iris = id
            )
          } else {
            if (size > 1) {
              println(s"union of $size irises for $id")
            }
            val geom = union(irises.values)
            val sampler = new PolygonSampler(geom.get)
            val coordinate = sampler.apply(rnd)
            val transformed = JTS.transform(coordinate, null, transformL93)
            val point = JTS.toGeometry(JTS.toDirectPosition(transformed, outCRS))
            Equipment(
              typeEquipment = typeEquipment,
              point = point,
              location = (point.getX, point.getY),
              quality = quality,
              iris = id
            )
          }
        }
      }
    }
  }
  def union(polygons: Iterable[MultiPolygon]) = {
    val geometryFactory = JTSFactoryFinder.getGeometryFactory()
    val union = geometryFactory.createGeometryCollection(polygons.toArray).union
    union match {
      case p: Polygon => Some(geometryFactory.createMultiPolygon(Array(p)))
      case mp: MultiPolygon => Some(mp)
      case _ => None
    }
  }

  def generateEquipments(inputDirectory: File, rng: Random) = {
    val BPEFile = inputDirectory / "bpe14-IDF.csv.lzma"
    val contourIRISFile = inputDirectory / "CONTOURS-IRIS_FE_IDF.shp"

    for {
      equipment <- readEquipment(BPEFile)
      geom <- readGeometry(contourIRISFile)
    } yield generateEquipment(rng, equipment, geom).toIterator
  }

  def sampleActivity(feature: Feature, rnd: RandomGenerator, distance: Double = 10000) = {
    val poisson = new PoissonDistribution(rnd, distance, PoissonDistribution.DEFAULT_EPSILON, PoissonDistribution.DEFAULT_MAX_ITERATIONS)
    val dist = poisson.sample.toDouble
    val angle = rnd.nextDouble * Math.PI * 2.0
    val p = feature.point
    val factory = p.getFactory
    val point = factory.createPoint(new Coordinate(p.getX + Math.cos(angle) * dist, p.getY + Math.sin(angle) * dist))
    Activity(
      point = point,
      location = (point.getX, point.getY)
    )
  }

  class RasterVariate(pdf: Seq[Double], val m_size: Seq[Int]) {
    val N = m_size.size
    val m_totalsSize = m_size.product
    val m_cdf = buildCdf(m_totalsSize, pdf, m_size).toParArray
    val m_sum = pdf.foldLeft(0.0)((a, b) => a + b)

    def buildCdf(totsize: Int, pdf: Seq[Double], size: Seq[Int]) = {
      var sum = 0.0
      var cdf = ArrayBuffer(sum)
      for (i <- 0 until totsize) {
        sum = sum + pdf(i)
        cdf.append(sum)
      }
      cdf = cdf.map(_ / sum)
      cdf.toIndexedSeq
    }

    def compute(rng: Random): Vector[Double] = {
      val x = rng.nextDouble()
      var offset = m_cdf.indexWhere(p => p > x) - 1
      //assert(offset>=0, s"$x within $m_cdf gives $offset")
      val output = ArrayBuffer.fill(N)(0.0)
      for (i <- 0 until N) {
        val ix = offset % m_size(i)
        output(i) = (ix + rng.nextDouble()) / m_size(i)
        offset /= m_size(i)
      }
      output.toVector
    }

  }
  class PolygonSampler(val polygon: MultiPolygon, val tolerance: Double = 0.1) {
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

