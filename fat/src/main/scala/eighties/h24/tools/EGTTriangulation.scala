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

import better.files._
import com.vividsolutions.jts.geom.GeometryFactory
import org.geotools.data.shapefile.ShapefileDataStoreFactory
import org.geotools.data.{DataUtilities, Transaction}

import eighties.h24.generation

object EGTTriangulation extends App {

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
      t <- generation.generateFlowsFromEGT(path)
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
