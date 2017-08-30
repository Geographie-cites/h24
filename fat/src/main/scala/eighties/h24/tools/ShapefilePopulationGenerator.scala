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
import com.vividsolutions.jts.geom.{Coordinate, GeometryFactory, MultiPolygon}
import com.vividsolutions.jts.index.strtree.STRtree
import eighties.h24.generation.{PolygonSampler, WorldFeature}
import eighties.h24.multinomial
import eighties.h24.population._
import eighties.h24.space.{Index, generateWorld}
import org.geotools.data.shapefile.{ShapefileDataStore, ShapefileDataStoreFactory}
import org.geotools.data.{DataUtilities, Transaction}

import scala.util.{Random, Try}

object ShapefilePopulationGenerator extends App {
  type Building = (MultiPolygon, Double)
  def index(aFile: File) = {
    val store = new ShapefileDataStore(aFile.toJava.toURI.toURL)
    try {
      val reader = store.getFeatureReader
      try {
        Try {
          val featureReader = Iterator.continually(reader.next).takeWhile(_ => reader.hasNext)
          var index = new STRtree()
          featureReader.foreach { feature =>
            val geom = feature.getDefaultGeometry.asInstanceOf[MultiPolygon]
            val height = feature.getAttribute("HAUTEUR").asInstanceOf[Integer].toDouble
            index.insert(geom.getEnvelopeInternal, (geom,height))
          }
          index
        }
      } finally reader.close
    } finally store.dispose
  }
  val inputFileName = "population.bin"
  val outputFileName = "population_buildings.shp"
  val buildingFile = File("buildings_laea.shp")
  val path = File("data")
  val outputPath = File("results")
  val seed = 42
  val rng = new Random(seed)
  outputPath.createDirectories
  val outFile = outputPath / outputFileName
  val specs = "geom:Point:srid=3035," +
              "cellX:Integer," +
              "cellY:Integer," +
              "ageCat:String," +
              "sex:String," +
              "education:String"
  val geometryFactory = new GeometryFactory
  val factory = new ShapefileDataStoreFactory
  val dataStore = factory.createDataStore(outFile.toJava.toURI.toURL)
  val featureTypeName = "Object"
  val featureType = DataUtilities.createType(featureTypeName, specs)
  dataStore.createSchema(featureType)
  val typeName = dataStore.getTypeNames()(0)
  val writer = dataStore.getFeatureWriterAppend(typeName, Transaction.AUTO_COMMIT)
  val worldFeature = WorldFeature.load(outputPath / inputFileName)
  println("bbox = " + worldFeature.boundingBox.minI + ", " + worldFeature.boundingBox.minJ )
  println("obbox = " + worldFeature.originalBoundingBox.minI + ", " + worldFeature.originalBoundingBox.minJ )
  println(Calendar.getInstance.getTime + " generating world")
  def healthCategory(a:AggregatedSocialCategory, b:Random) = HealthCategory(0.0,Healthy,ChangeConstraints(false,false,false))
  val world = generateWorld(worldFeature.individualFeatures, healthCategory, rng)
  val bbox = worldFeature.originalBoundingBox
  val indexedWorld = Index.indexIndividuals(world, Individual.home.get)
  val locCells = Index.getLocatedCells(indexedWorld)
  println(Calendar.getInstance.getTime + " indexing buidings")
  val buildingIndex = index(buildingFile).get
  println(Calendar.getInstance.getTime + " interating though cells")
  for {
    (features, (i,j)) <- locCells
  } {
    if (features.size > 0) {
      val cellgeom = geometryFactory.createPolygon(Array(
        new Coordinate((bbox.minI + i.toDouble) * 1000.0, (bbox.minJ + j.toDouble) * 1000.0),
        new Coordinate((bbox.minI + i.toDouble + 1.0) * 1000.0, (bbox.minJ + j.toDouble) * 1000.0),
        new Coordinate((bbox.minI + i.toDouble + 1.0) * 1000.0, (bbox.minJ + j.toDouble + 1.0) * 1000.0),
        new Coordinate((bbox.minI + i.toDouble) * 1000.0, (bbox.minJ + j.toDouble + 1.0) * 1000.0),
        new Coordinate((bbox.minI + i.toDouble) * 1000.0, (bbox.minJ + j.toDouble) * 1000.0)
      ))
      val relevant = buildingIndex.query(cellgeom.getEnvelopeInternal).toArray.toSeq.
        map(_.asInstanceOf[Building]).filter(_._1.intersects(cellgeom))
      val relevantBuildingVolumes = relevant.map {
        b => {
          val g = b._1.intersection(cellgeom)
          (b, b._2 * g.getArea)
        }
      }.toVector.filter { case (_, v) => v > 0 }
      if (relevantBuildingVolumes.isEmpty) {
        println("NoOOOOOOooooOOO building intersecting the cell")
        println("cell " + i + ", " + j + " = " + cellgeom.toText)
        println("ignoring " + features.size + " features")
        //throw new RuntimeException("NoOOOOOOooooOOO building intersecting the cell")
      } else {
        features.foreach(indiv => {
          val select = multinomial(relevantBuildingVolumes)(rng)
          val sampler = new PolygonSampler(select._1)
          val p = sampler(rng)
          val codes = AggregatedSocialCategory.toCode(indiv.socialCategory)
          val values = Array[AnyRef](
            geometryFactory.createPoint(p),
            indiv.home._1.asInstanceOf[AnyRef],
            indiv.home._2.asInstanceOf[AnyRef],
            codes(1).asInstanceOf[AnyRef],
            codes(0).asInstanceOf[AnyRef],
            codes(2).asInstanceOf[AnyRef]
          )
          val simpleFeature = writer.next
          simpleFeature.setAttributes(values)
          writer.write
        })
      }
    }
  }
  writer.close
  dataStore.dispose
}
