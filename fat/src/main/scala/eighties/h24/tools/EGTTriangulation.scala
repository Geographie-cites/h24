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
import com.vividsolutions.jts.geom.{Coordinate, GeometryCollection, GeometryFactory}
import com.vividsolutions.jts.triangulate.ConformingDelaunayTriangulationBuilder
import org.geotools.data.shapefile.ShapefileDataStoreFactory
import org.geotools.data.{DataUtilities, Transaction}
import com.github.tototoshi.csv.{CSVFormat, CSVReader, DefaultCSVFormat}
import org.apache.commons.compress.compressors.lzma.LZMACompressorInputStream

import scala.io.Source
import scala.util.Try

object EGTTriangulation extends App {
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

  def readResidenceFromEGT(aFile: File) = withCSVReader(aFile)(SemicolonFormat){ reader =>
    Try {
      reader.iterator.drop(1).filter(l => l(11).equalsIgnoreCase("1") && !l(19).equalsIgnoreCase("NA") && !l(20).equalsIgnoreCase("NA")).map { line =>
        val carreau = line(1).trim
        val x = line(19).trim.replaceAll(",",".").toDouble
        val y = line(20).trim.replaceAll(",",".").toDouble
        carreau -> new Coordinate(x,y)
      }.toMap
    }
  }

  def generateFlowsFromEGT(inputDirectory: File,tolerance: Double = 1.0): Try[GeometryCollection]= {
    val presenceFile = inputDirectory / "presence_semaine_GLeRoux.csv.lzma"
    val r = for {
      m <- readResidenceFromEGT(presenceFile)
    } yield {
      val builder = new ConformingDelaunayTriangulationBuilder
      val v = m.values
      val geomFactory = new GeometryFactory
      val mp = geomFactory.createMultiPoint(v.toArray)
      builder.setSites(mp)
      builder.setTolerance(tolerance)
      builder.getTriangles(geomFactory).asInstanceOf[GeometryCollection]
    }
    r
  }

  val path = File("../données/EGT 2010/presence semaine EGT")
  val outputPath = File("results")
  outputPath.createDirectories()

  val outFile = outputPath / "EGT_Triangulation.shp"

  val specs = "geom:Polygon:srid=27572"

    val factory = new ShapefileDataStoreFactory
    val geomfactory = new GeometryFactory
    val dataStore = factory.createDataStore(outFile.toJava.toURI.toURL)
    val featureTypeName = "Triangulation"
    val featureType = DataUtilities.createType(featureTypeName, specs)
    dataStore.createSchema(featureType)
    val typeName = dataStore.getTypeNames()(0)
    val writer = dataStore.getFeatureWriterAppend(typeName, Transaction.AUTO_COMMIT)

  val res =
    for {
      t <- generateFlowsFromEGT(path)
    } yield {
    val num = t.getNumGeometries
    for (i <- 0 until num) {
      val g = t.getGeometryN(i)
      val values = Array[AnyRef](g)
      val simpleFeature = writer.next
      simpleFeature.setAttributes(values)
      writer.write
    }
  }
  writer.close

  println(res.get)
}
