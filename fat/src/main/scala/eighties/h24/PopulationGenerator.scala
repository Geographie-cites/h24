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

import better.files._
import com.vividsolutions.jts.geom.Coordinate
import org.apache.commons.math3.random.JDKRandomGenerator
import org.geotools.data.shapefile.ShapefileDataStoreFactory
import org.geotools.data.{DataUtilities, Transaction}

object PopulationGenerator extends App {

  val path = File("data")
  val outputPath = File("results")
  outputPath.createDirectories()

  val outFile = outputPath / "generated-population-75-work.shp"

  val specs = "geom:Point:srid=3035,mainactiv:Point:srid=3035,cellX:Integer,cellY:Integer,ageCat:Integer,age:Double,sex:Integer,education:Integer,work:Point:srid=3035"
  val factory = new ShapefileDataStoreFactory
  val dataStore = factory.createDataStore(outFile.toJava.toURI.toURL)
  val featureTypeName = "Object"
  val featureType = DataUtilities.createType(featureTypeName, specs)
  dataStore.createSchema(featureType)
  val typeName = dataStore.getTypeNames()(0)
  val writer = dataStore.getFeatureWriterAppend(typeName, Transaction.AUTO_COMMIT)
  val rng = new JDKRandomGenerator(42)

  for {
    (feature, i) <- generation.generateFeatures(path.toJava, _ => true, rng).get.zipWithIndex
  } {
    import feature._
    val activity = generation.sampleActivity(feature, rng, 10000)
    def discrete(v:Double) = (v / 200.0).toInt
    val mainActivityPoint = mainActivity match {
      case Some(p) => point.getFactory.createPoint(new Coordinate(p._1, p._2))
      case None => null
    }
    val values = Array[AnyRef](
      point,
      mainActivityPoint,
      discrete(location._1).asInstanceOf[AnyRef],
      discrete(location._2).asInstanceOf[AnyRef],
      ageCategory.asInstanceOf[AnyRef],
      age.getOrElse(75).asInstanceOf[AnyRef],
      sex.asInstanceOf[AnyRef],
      education.asInstanceOf[AnyRef],
      activity.point
    )
    val simpleFeature = writer.next
    simpleFeature.setAttributes(values)
    writer.write
  }
  writer.close
}
