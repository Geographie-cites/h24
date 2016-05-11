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
import com.vividsolutions.jts.geom.{Geometry, MultiPolygon, Polygon}
import eighties.geometry.PolygonSampler
import org.apache.commons.compress.compressors.lzma.LZMACompressorInputStream
import org.geotools.data.{DataUtilities, Transaction}
import org.geotools.data.shapefile.{ShapefileDataStore, ShapefileDataStoreFactory}
import org.geotools.geometry.jts.JTS
import org.geotools.referencing.CRS

import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import scala.util.{Random, Try}
import better.files._

package object generation {

  type IrisID = String
  case class Iris(id: IrisID, geometry: Geometry, population: Int, ageSex: Vector[Vector[Double]], educationSex: Vector[Vector[Double]])


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
         .filter(feature =>feature.getAttribute("DCOMIRIS").toString.startsWith("78"))
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
        reader.iterator.drop(6).map { line =>
          val men = line.drop(36).take(7).map(toDouble).toVector
          val women = line.drop(44).take(7).map(toDouble).toVector
          //line(0) -> line.drop(35).take(16).grouped(8).map(_.toVector.map(toDouble)).toVector
          line(0) -> Vector(men, women)
        }.toMap
      }
    reader.close
    result
  }

  def readAgeSex(stream: InputStream) = {
    val reader = CSVReader.open(Source.fromInputStream(stream))
    val result =
      Try {
        reader.iterator.drop(6).map { line =>
          val men = line.drop(34).take(6).map(toDouble).toVector
          val women = line.drop(44).take(6).map(toDouble).toVector
          line(0) -> (men ++ women)
        }.toMap
      }
    reader.close
    result
  }

  def generatePopulation(rnd: Random, geometry: Map[String, Polygon], ageSex: Map[String, Vector[Double]], educationSex: Map[String, Vector[Vector[Double]]]) = {
    geometry.toSeq.map {
      case (id, geom) => {
        val sampler = new PolygonSampler(geom)
        val ageSexV = ageSex.get(id).get
        val total = ageSexV.sum
        val educationSexV = educationSex.get(id).get

        val ageSexSizes = Seq(6,2)
        val ageSexVariate = new RasterVariate(ageSexV, ageSexSizes)

        val educationSexSizes = Seq(7)
        val educationSexVariates = ArrayBuffer(
          new RasterVariate(educationSexV(0), educationSexSizes),
          new RasterVariate(educationSexV(1), educationSexSizes))

        val res = (0 to total.toInt).map{_=>
          val sample = ageSexVariate.compute(rnd)
          val age = (sample(0)*ageSexSizes(0)).toInt
          val sex = (sample(1)*ageSexSizes(1)).toInt
          val education = if (age>0) {
            (educationSexVariates(sex).compute(rnd)(0) * educationSexSizes(0)).toInt
          } else -1
          (age,sex,education,geom.getFactory.createPoint(sampler.apply(rnd)))
        }
        res
      }
    }
  }

  def generateIndividuals(inputDirectory: File, rng: Random) = {
    val contourIRISFile = inputDirectory / "CONTOURS-IRIS_FE_IDF.shp"
    val baseICEvolStructPopFileName = inputDirectory / "base-ic-evol-struct-pop-2012-IDF.csv.lzma"
    val baseICDiplomesFormationPopFileName = inputDirectory / "base-ic-diplomes-formation-2012-IDF.csv.lzma"
    val outFileName = inputDirectory / "generated-population-78.shp"
    val specs = "geomLAEA:Point:srid=3035,cellX:Integer,cellY:Integer,age:Integer,sex:Integer,education:Integer"
    val factory = new ShapefileDataStoreFactory
    val inBaseICEvolStructPop = new BufferedInputStream(new FileInputStream(baseICEvolStructPopFileName.toJava))
    val inBaseICDiplomesFormationPop = new BufferedInputStream(new FileInputStream(baseICDiplomesFormationPopFileName.toJava))
    for {
      geom <- readGeometry(contourIRISFile)
      ageSex <- readAgeSex(new LZMACompressorInputStream(inBaseICEvolStructPop))
      educationSex <- readEducationSex(new LZMACompressorInputStream(inBaseICDiplomesFormationPop))
    } yield generatePopulation(rng, geom, ageSex, educationSex).toIterator.flatten
  }

}
