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
package eighties.h24.tools

import java.io.{BufferedInputStream, FileInputStream}

import better.files._
import com.github.tototoshi.csv.{CSVFormat, CSVReader, DefaultCSVFormat}
import com.vividsolutions.jts.geom.{Coordinate, GeometryFactory}
import org.apache.commons.compress.compressors.lzma.LZMACompressorInputStream
import org.apache.commons.math3.random.JDKRandomGenerator
import org.geotools.data.shapefile.ShapefileDataStoreFactory
import org.geotools.data.{DataUtilities, Transaction}

import scala.io.Source
import scala.util.Try

object EGTAnalysis extends App {

  val path = File("../données/EGT 2010/presence semaine EGT")
  val outputPath = File("results")
  outputPath.createDirectories()

  val outFileRes = outputPath / "EGT_RES.shp"
  val outFileAct = outputPath / "EGT_ACT.shp"
  val outFileDep = outputPath / "EGT_DEP.shp"

  val specsRes = "geom:Point:srid=27572," +
    "ID_pers:String," +
    "sexe:Integer," +
    "age:Integer," +
    "dipl:String," +
    "cs8:String," +
    "cs24l:String"
  val specsAct = "geom:Point:srid=27572," +
    "ID_pers:String," +
    "sexe:Integer," +
    "age:Integer," +
    "dipl:String," +
    "cs8:String," +
    "cs24l:String," +
    "motif:String"
  val specsDep = "geom:LineString:srid=27572," +
    "ID_pers:String," +
    "sexe:Integer," +
    "age:Integer," +
    "dipl:String," +
    "cs8:String," +
    "cs24l:String," +
    "motif:String"

  object CommaFormat extends DefaultCSVFormat {
    override val delimiter = ','
  }
  object SemicolonFormat extends DefaultCSVFormat {
    override val delimiter = ';'
  }
  def withCSVReader[T](file: File)(format: CSVFormat)(f: CSVReader => T) = {
    val in = new BufferedInputStream(new FileInputStream(file.toJava))
    val stream = new LZMACompressorInputStream(in)
    val reader = CSVReader.open(Source.fromInputStream(stream, "ISO-8859-1"))(format)
    try f(reader)
    finally reader.close
  }

  def readEGT(file: File) = withCSVReader(file)(SemicolonFormat) { reader =>
    Try {
      reader.iterator.drop(1).filter(l=> !l(21).trim.equalsIgnoreCase("NA")).map { line =>
        val id_pers = line(0).trim
        val motif = line(11)
        val sexe = line(12)
        val age = line(13)
        val dipl = line(14)
        val cs8 = line(15)
        val cs24l = line(16)
        val px = line(17).replace(',', '.')
        val py = line(18).replace(',', '.')
        val resx = line(19).replace(',', '.')
        val resy = line(20).replace(',', '.')
        val dist = line(21).replace(',', '.')
        id_pers -> Vector(motif, sexe, age, dipl, cs8, cs24l, px, py, resx, resy, dist)
      }.toVector
    }
  }

    val factory = new ShapefileDataStoreFactory
    val geomfactory = new GeometryFactory
    val dataStoreRes = factory.createDataStore(outFileRes.toJava.toURI.toURL)
    val featureTypeNameRes = "Res"
    val featureTypeRes = DataUtilities.createType(featureTypeNameRes, specsRes)
    dataStoreRes.createSchema(featureTypeRes)
    val typeNameRes = dataStoreRes.getTypeNames()(0)
    val writerRes = dataStoreRes.getFeatureWriterAppend(typeNameRes, Transaction.AUTO_COMMIT)

    val dataStoreAct = factory.createDataStore(outFileAct.toJava.toURI.toURL)
    val featureTypeNameAct = "Act"
    val featureTypeAct = DataUtilities.createType(featureTypeNameAct, specsAct)
    dataStoreAct.createSchema(featureTypeAct)
    val typeNameAct = dataStoreAct.getTypeNames()(0)
    val writerAct = dataStoreAct.getFeatureWriterAppend(typeNameAct, Transaction.AUTO_COMMIT)

    val dataStoreDep = factory.createDataStore(outFileDep.toJava.toURI.toURL)
    val featureTypeNameDep = "Dep"
    val featureTypeDep = DataUtilities.createType(featureTypeNameDep, specsDep)
    dataStoreDep.createSchema(featureTypeDep)
    val typeNameDep = dataStoreDep.getTypeNames()(0)
    val writerDep = dataStoreDep.getFeatureWriterAppend(typeNameDep, Transaction.AUTO_COMMIT)

  val rng = new JDKRandomGenerator(42)
  for {
    (p, i) <- readEGT(path / "presence_semaine_GLeRoux.csv.lzma").get.zipWithIndex
  }{
    val id = p._1
    val values = p._2
    val actX = values(6).toDouble
    val actY = values(7).toDouble
    val resX = values(8).toDouble
    val resY = values(9).toDouble
    val resPoint = geomfactory.createPoint(new Coordinate(resX,resY))
    val actPoint = geomfactory.createPoint(new Coordinate(actX,actY))
    val depl = geomfactory.createLineString(Array(new Coordinate(resX,resY),new Coordinate(actX,actY)))
    val motif = values(0)
    val sexe = values(1).toInt
    val age = values(2).toInt
    val dipl = values(3)
    val cs8 = values(4)
    val cs24l = values(5)
    val dist = values(10).toDouble
    val valuesRes = Array[AnyRef](
      resPoint,
      id.asInstanceOf[AnyRef],
      sexe.asInstanceOf[AnyRef],
      age.asInstanceOf[AnyRef],
      dipl.asInstanceOf[AnyRef],
      cs8.asInstanceOf[AnyRef],
      cs24l.asInstanceOf[AnyRef]
    )
    val simpleFeatureRes = writerRes.next
    simpleFeatureRes.setAttributes(valuesRes)
    val valuesAct = Array[AnyRef](
      actPoint,
      id.asInstanceOf[AnyRef],
      sexe.asInstanceOf[AnyRef],
      age.asInstanceOf[AnyRef],
      dipl.asInstanceOf[AnyRef],
      cs8.asInstanceOf[AnyRef],
      cs24l.asInstanceOf[AnyRef],
      motif.asInstanceOf[AnyRef]
    )
    val simpleFeatureAct = writerAct.next
    simpleFeatureAct.setAttributes(valuesAct)
    val valuesDep = Array[AnyRef](
      depl,
      id.asInstanceOf[AnyRef],
      sexe.asInstanceOf[AnyRef],
      age.asInstanceOf[AnyRef],
      dipl.asInstanceOf[AnyRef],
      cs8.asInstanceOf[AnyRef],
      cs24l.asInstanceOf[AnyRef],
      motif.asInstanceOf[AnyRef]
    )
    val simpleFeatureDep = writerDep.next
    simpleFeatureDep.setAttributes(valuesDep)
    writerRes.write
    writerAct.write
    writerDep.write
  }
  writerRes.close
  writerAct.close
  writerDep.close
}
