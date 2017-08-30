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

import java.util.Calendar

import better.files._
import com.vividsolutions.jts.geom.{Coordinate, GeometryFactory}
import eighties.h24.generation.WorldFeature
import org.geotools.data.shapefile.ShapefileDataStoreFactory
import org.geotools.data.{DataUtilities, Transaction}
import org.geotools.feature.DefaultFeatureCollection
import org.geotools.feature.simple.SimpleFeatureBuilder
import org.geotools.geojson.feature.FeatureJSON
import org.geotools.referencing.CRS

object GeoJSONPopulationGenerator extends App {
  val inputFileName = "population2.bin"
  val outputFileName = "population2.json"
  val path = File("data")
  val outputPath = File("results")
  outputPath.createDirectories
  val outFile = outputPath / outputFileName
  val specs = "geom:Point:srid=3035," +
//              "cellX:Integer," +
//              "cellY:Integer," +
              "ageCat:Integer," +
              "sex:Integer," +
              "education:Integer"
  val crs = CRS.decode( "EPSG:3035" )
  val featureTypeName = "Individual"
  val featureType = DataUtilities.createType(featureTypeName, specs)
  val featureCollection = new DefaultFeatureCollection(featureTypeName, featureType)
  println(Calendar.getInstance.getTime + " Loading population")
  val res = WorldFeature.load(outputPath / inputFileName)
  println(Calendar.getInstance.getTime + " Converting population")
  val geometryFactory = new GeometryFactory
  for {
    (feature, i) <- res.individualFeatures.zipWithIndex
  } {
    import feature._
    val point = geometryFactory.createPoint(new Coordinate(location._1.toDouble + 500.0, location._2.toDouble + 500.0))
    point.setUserData(crs)
    val values = Array[AnyRef](
      point,
//      location._1.asInstanceOf[AnyRef],
//      location._2.asInstanceOf[AnyRef],
      ageCategory.asInstanceOf[AnyRef],
      sex.asInstanceOf[AnyRef],
      education.asInstanceOf[AnyRef]
    )
    featureCollection.add( SimpleFeatureBuilder.build( featureType, values, null) )
  }
  println(Calendar.getInstance.getTime + " Writing population")
  val io = new FeatureJSON
  println(featureCollection.getBounds.getCoordinateReferenceSystem)
  io.writeFeatureCollection(featureCollection, outFile.toJava)
  println(Calendar.getInstance.getTime + " Finished")
}
  