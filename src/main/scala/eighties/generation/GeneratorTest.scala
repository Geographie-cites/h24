package eighties.generation

import java.io.{BufferedInputStream, FileInputStream}

import org.apache.commons.compress.compressors.lzma.LZMACompressorInputStream
import org.geotools.data.{DataUtilities, Transaction}
import org.geotools.data.shapefile.ShapefileDataStoreFactory
import org.geotools.geometry.jts.JTS
import org.geotools.referencing.CRS
import better.files._

import scala.util.Random

object GeneratorTest extends App {

  val path = File("data")
  val outputPath = File("results")
  outputPath.createDirectories()

  val outFile = outputPath / "generated-population-78.shp"

  val inCRS = CRS.decode("EPSG:2154")
  val outCRS = CRS.decode("EPSG:3035")
  val transform = CRS.findMathTransform(inCRS, outCRS, true)
//geom:Point:srid=2154,
  val specs = "geomLAEA:Point:srid=3035,cellX:Integer,cellY:Integer,age:Integer,sex:Integer,education:Integer"
  val factory = new ShapefileDataStoreFactory
  val dataStore = factory.createDataStore(outFile.toJava.toURI.toURL)
  val featureTypeName = "Object"
  val featureType = DataUtilities.createType(featureTypeName, specs)
  dataStore.createSchema(featureType)
  val typeName = dataStore.getTypeNames()(0)
  val writer = dataStore.getFeatureWriterAppend(typeName, Transaction.AUTO_COMMIT)
  val rng = new Random(42)

  for {
    ((age,sex,education,point), i) <- generateIndividuals(path, rng).get.zipWithIndex
  } yield {
    val transformedPoint = JTS.transform(point, transform)
    def discrete(v:Double) = (v/200.0).toInt * 200
    val values = Array[AnyRef](
      transformedPoint,
      discrete(transformedPoint.getCoordinate.x).asInstanceOf[AnyRef],
      discrete(transformedPoint.getCoordinate.y).asInstanceOf[AnyRef],
      age.asInstanceOf[AnyRef],
      sex.asInstanceOf[AnyRef],
      education.asInstanceOf[AnyRef])
    val simpleFeature = writer.next
    simpleFeature.setAttributes(values)
    writer.write
  }

  writer.close
}
