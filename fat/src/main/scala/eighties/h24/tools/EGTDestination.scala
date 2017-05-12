package eighties.h24.tools

import better.files.File
import com.vividsolutions.jts.geom.{Coordinate, GeometryFactory}
import eighties.h24.dynamic.MoveMatrix
import eighties.h24.generation.{Flow, WorldFeature}
import eighties.h24.space.BoundingBox
import eighties.h24.{generation, space}
import org.geotools.data.shapefile.ShapefileDataStoreFactory
import org.geotools.data.{DataUtilities, Transaction}
import org.geotools.geometry.jts.JTS
import org.geotools.referencing.CRS

/**
  */
object EGTDestination extends App {
  def flowsFromEGT(bb: BoundingBox, aFile: File, res: File) = {
    val l2eCRS = CRS.decode("EPSG:27572")
    val outCRS = CRS.decode("EPSG:3035")
    val transform = CRS.findMathTransform(l2eCRS, outCRS, true)
//    val x_laea_min = 3697000
//    val x_laea_max = 3846000
//    val y_laea_min = 2805000
//    val y_laea_max = 2937000
    //val row = (x_laea_max - x_laea_min) / 1000
    def location(coord: Coordinate): space.Location = {
      val laea_coord = JTS.transform(coord, null, transform)
      // replace by cell...
      val dx = (laea_coord.x - bb.minI)
      val dy = (laea_coord.y - bb.minJ)
      //      dx+dy*row
      space.cell(dx, dy)
    }
    //val formatter = new SimpleDateFormat("dd/MM/yy hh:mm")
    //val startDate = new DateTime(formatter.parse("01/01/2010 04:00"))

    val factory = new ShapefileDataStoreFactory
    val geomfactory = new GeometryFactory

    //val resx=64
    //val resy=81
    val file = DataUtilities.urlToFile(res.toJava.toURI.toURL)
    val dataStoreRes = factory.createDataStore(res.toJava.toURI.toURL)
    val featureTypeNameRes = "Res"
    val specsRes = "geom:Point:srid=3035"
    val featureTypeRes = DataUtilities.createType(featureTypeNameRes, specsRes)
    dataStoreRes.createSchema(featureTypeRes)
    val typeNameRes = dataStoreRes.getTypeNames()(0)
    val writerRes = dataStoreRes.getFeatureWriterAppend(typeNameRes, Transaction.AUTO_COMMIT)

    val path = File("../data/EGT 2010/presence semaine EGT")
    val newMatrix = generation.flowsFromEGT(bb, path / "presence_semaine_GLeRoux.csv.lzma").get

    MoveMatrix.allMoves.getAll(newMatrix).toSeq.map {
      v=>
      val loc = v._1
        val valuesRes = Array[AnyRef](
          geomfactory.createPoint(new Coordinate(bb.minI + loc._1 * 1000 + 500.0, bb.minJ + loc._2 * 1000 + 500.0))
        )
        val simpleFeatureRes = writerRes.next
        simpleFeatureRes.setAttributes(valuesRes)
        writerRes.write
    }

    writerRes.close
  }
  val path = File("../data/EGT 2010/presence semaine EGT")
  val outputPath = File("results")
  outputPath.createDirectories()
  val outFileRes = outputPath / "TEST_DEST_IDW.shp"
  def features = WorldFeature.load(File("results/population.bin"))
  flowsFromEGT(features.originalBoundingBox, path / "presence_semaine_GLeRoux.csv.lzma",outFileRes)
}
