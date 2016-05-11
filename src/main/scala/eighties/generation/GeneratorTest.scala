package eighties.generation

import java.io.{BufferedInputStream, File, FileInputStream}

import org.apache.commons.compress.compressors.lzma.LZMACompressorInputStream
import org.geotools.data.{DataUtilities, Transaction}
import org.geotools.data.shapefile.ShapefileDataStoreFactory
import org.geotools.geometry.jts.JTS
import org.geotools.referencing.CRS

import scala.util.Random

object GeneratorTest extends App {
  val path = "data"
  val outputPath = "results"
  new File(outputPath).mkdirs()
  val contourIRISFileName = s"${path}/CONTOURS-IRIS_FE_IDF.shp"
  val baseICEvolStructPopFileName = s"${path}/base-ic-evol-struct-pop-2012-IDF.csv.lzma"
  val baseICDiplomesFormationPopFileName = s"${path}/base-ic-diplomes-formation-2012-IDF.csv.lzma"
  val outFileName = s"${outputPath}/generated-population-78.shp"

  val inCRS = CRS.decode("EPSG:2154")
  val outCRS = CRS.decode("EPSG:3035")
  val transform = CRS.findMathTransform(inCRS, outCRS, true)
//geom:Point:srid=2154,
  val specs = "geomLAEA:Point:srid=3035,cellX:Integer,cellY:Integer,age:Integer,sex:Integer,education:Integer"
  val factory = new ShapefileDataStoreFactory
  val dataStore = factory.createDataStore(new File(outFileName).toURI().toURL())
  val featureTypeName = "Object"
  val featureType = DataUtilities.createType(featureTypeName, specs)
  dataStore.createSchema(featureType)
  val typeName = dataStore.getTypeNames()(0)
  val writer = dataStore.getFeatureWriterAppend(typeName, Transaction.AUTO_COMMIT)
  val rng = new Random(42)
  val finBaseICEvolStructPop = new FileInputStream(baseICEvolStructPopFileName)
  val inBaseICEvolStructPop = new BufferedInputStream(finBaseICEvolStructPop)
  val finBaseICDiplomesFormationPop = new FileInputStream(baseICDiplomesFormationPopFileName)
  val inBaseICDiplomesFormationPop = new BufferedInputStream(finBaseICDiplomesFormationPop)
  for {
    geom <- readGeometry(new File(contourIRISFileName))
    ageSex <- readAgeSex(new LZMACompressorInputStream(inBaseICEvolStructPop))
    educationSex <- readEducationSex(new LZMACompressorInputStream(inBaseICDiplomesFormationPop))
  } yield {
    generatePopulation(rng, geom, ageSex, educationSex).toIterator.flatten.zipWithIndex.foreach {
      case ((age, sex, education, point), i) =>
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
}
