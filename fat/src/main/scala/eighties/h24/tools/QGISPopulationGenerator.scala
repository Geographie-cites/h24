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
import com.vividsolutions.jts.geom.{Coordinate, GeometryFactory}
import eighties.h24.generation
import eighties.h24.generation.IndividualFeature
import org.apache.commons.math3.random.JDKRandomGenerator
import org.geotools.data.shapefile.ShapefileDataStoreFactory
import org.geotools.data.{DataUtilities, Transaction}

object QGISPopulationGenerator extends App {

  val path = File("data")
  val outputPath = File("results")
  outputPath.createDirectories()

  val outFile = outputPath / "generated-population.shp"

  val specs = "geom:Point:srid=3035," +
              "cellX:Integer," +
              "cellY:Integer," +
              "ageCat:Integer," +
              "sex:Integer," +
              "education:Integer"
  val geomfact = new GeometryFactory
  val factory = new ShapefileDataStoreFactory
  val dataStore = factory.createDataStore(outFile.toJava.toURI.toURL)
  val featureTypeName = "Object"
  val featureType = DataUtilities.createType(featureTypeName, specs)
  dataStore.createSchema(featureType)
  val typeName = dataStore.getTypeNames()(0)
  val writer = dataStore.getFeatureWriterAppend(typeName, Transaction.AUTO_COMMIT)
  val rng = new JDKRandomGenerator(42)
  def filterParis13 (v:String) = v.startsWith("75113")
  def filterAll (v:String) = true
  def filter (v:String) = filterParis13(v)
  val res = IndividualFeature.load(File("results/population.bin"))
  for {
    (feature, i) <- res.zipWithIndex
  } {
    import feature._
    val values = Array[AnyRef](
      geomfact.createPoint(new Coordinate(location._1.toDouble + 500.0, location._2.toDouble + 500.0)),
      location._1.asInstanceOf[AnyRef],
      location._2.asInstanceOf[AnyRef],
      ageCategory.asInstanceOf[AnyRef],
      sex.asInstanceOf[AnyRef],
      education.asInstanceOf[AnyRef]
    )
    val simpleFeature = writer.next
    simpleFeature.setAttributes(values)
    writer.write
  }
  writer.close
}
