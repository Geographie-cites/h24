package eighties.h24.tools

import java.text.SimpleDateFormat

import better.files.File
import eighties.h24.generation.Flow
import eighties.h24.{generation, space}
import com.vividsolutions.jts.geom.{Coordinate, GeometryFactory}
import org.geotools.data.shapefile.ShapefileDataStoreFactory
import org.geotools.data.{DataUtilities, Transaction}
import org.geotools.geometry.jts.JTS
import org.geotools.referencing.CRS
import org.joda.time.{DateTime, Interval}
import org.joda.time.format.DateTimeFormat

/**
  */
object EGTTest extends App {
  def flowsFromEGT(aFile: File, res: File) = {
    val l2eCRS = CRS.decode("EPSG:27572")
    val outCRS = CRS.decode("EPSG:3035")
    val transform = CRS.findMathTransform(l2eCRS, outCRS, true)
    val x_laea_min = 3697000
    val x_laea_max = 3846000
    val y_laea_min = 2805000
    val y_laea_max = 2937000
    //val row = (x_laea_max - x_laea_min) / 1000
    def location(coord: Coordinate): space.Location = {
      val laea_coord = JTS.transform(coord, null, transform)
      // replace by cell...
      val dx = (laea_coord.x - x_laea_min)
      val dy = (laea_coord.y - y_laea_min)
      //      dx+dy*row
      space.cell(dx, dy)
    }
    val formatter = new SimpleDateFormat("dd/MM/yy hh:mm")
    val startDate = new DateTime(formatter.parse("01/01/2010 04:00"))

    val factory = new ShapefileDataStoreFactory
    val geomfactory = new GeometryFactory

    val resx=64
    val resy=81
    println(res)
    val file = DataUtilities.urlToFile(res.toJava.toURI.toURL)
    println(file.isFile)
    val dataStoreRes = factory.createDataStore(res.toJava.toURI.toURL)
    println(dataStoreRes)
    val featureTypeNameRes = "Res"
    val specsRes = "geom:Point:srid=3035,start:String"
    println(specsRes)
    val featureTypeRes = DataUtilities.createType(featureTypeNameRes, specsRes)
    dataStoreRes.createSchema(featureTypeRes)
    val typeNameRes = dataStoreRes.getTypeNames()(0)
    val writerRes = dataStoreRes.getFeatureWriterAppend(typeNameRes, Transaction.AUTO_COMMIT)

    for {
      plage <- (0 until 24)
    } yield {
      val index = space.Index[Flow](
        generation.readFlowsFromEGT(aFile, location, /*age, sex, education, */new Interval(startDate.plusHours(plage),startDate.plusHours(plage+1))).get,
        (_: Flow).residence,
        149,
        132
      )
      val c = index.cells(resx)(resy)
      val values = c.map(f=>(f.activity, plage))
      values.map(v=>{
        val d = startDate.plusHours(v._2)
        val fmt = DateTimeFormat.forPattern("yyyy-MM-dd HH:mm:ss");
        val valuesRes = Array[AnyRef](
          geomfactory.createPoint(new Coordinate(x_laea_min+v._1._1*1000, y_laea_min+v._1._2*1000)),
          d.toString(fmt)
        )
        val simpleFeatureRes = writerRes.next
        simpleFeatureRes.setAttributes(valuesRes)
        writerRes.write
    })
    }
    writerRes.close
  }
  val path = File("../données/EGT 2010/presence semaine EGT")
  val outputPath = File("results")
  outputPath.createDirectories()
  val outFileRes = outputPath / "OUT_laea.shp"
  flowsFromEGT(path / "presence_semaine_GLeRoux.csv.lzma",outFileRes)
}
