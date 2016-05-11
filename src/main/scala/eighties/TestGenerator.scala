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

import java.io.{BufferedInputStream, FileInputStream}

import org.apache.commons.compress.compressors.lzma.LZMACompressorInputStream
import org.geotools.data.{DataUtilities, Transaction}
import org.geotools.data.shapefile.ShapefileDataStoreFactory
import org.geotools.geometry.jts.JTS
import org.geotools.referencing.CRS
import better.files._

import scala.util.Random

object TestGenerator extends App {

  val path = File("data")
  val outputPath = File("results")
  outputPath.createDirectories()

  val outFile = outputPath / "generated-population-75.shp"

  val inCRS = CRS.decode("EPSG:2154")
  val outCRS = CRS.decode("EPSG:3035")
  val transform = CRS.findMathTransform(inCRS, outCRS, true)
  //geom:Point:srid=2154,
  val specs = "geomLAEA:Point:srid=3035,cellX:Integer,cellY:Integer,ageCategory:Integer,age:Double,sex:Integer,education:Integer"
  val factory = new ShapefileDataStoreFactory
  val dataStore = factory.createDataStore(outFile.toJava.toURI.toURL)
  val featureTypeName = "Object"
  val featureType = DataUtilities.createType(featureTypeName, specs)
  dataStore.createSchema(featureType)
  val typeName = dataStore.getTypeNames()(0)
  val writer = dataStore.getFeatureWriterAppend(typeName, Transaction.AUTO_COMMIT)
  val rng = new Random(42)

  for {
    (feature, i) <- generation.generateFeatures(path, rng).get.zipWithIndex
  } {
    import feature._
    val transformedPoint = JTS.transform(point, transform)
    def discrete(v:Double) = (v / 200.0).toInt * 200
    val values = Array[AnyRef](
      transformedPoint,
      discrete(transformedPoint.getCoordinate.x).asInstanceOf[AnyRef],
      discrete(transformedPoint.getCoordinate.y).asInstanceOf[AnyRef],
      ageCategory.asInstanceOf[AnyRef],
      age.getOrElse(75).asInstanceOf[AnyRef],
      sex.asInstanceOf[AnyRef],
      education.asInstanceOf[AnyRef])
    val simpleFeature = writer.next
    simpleFeature.setAttributes(values)
    writer.write
  }

  writer.close
}
