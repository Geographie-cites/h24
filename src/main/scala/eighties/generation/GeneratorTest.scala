package eighties.generation

import java.io.File

import org.geotools.data.{DataUtilities, Transaction}
import org.geotools.data.shapefile.ShapefileDataStoreFactory

import scala.util.Random

object GeneratorTest extends App {
  val path = args(0)
  val contourIRISFileName = s"${path}/CONTOURS-IRIS_FE_IDF.shp"
  val baseICEvolStructPopFileName = s"${path}/base-ic-evol-struct-pop-2012-IDF.csv"
  val baseICDiplomesFormationPopFileName = s"${path}/base-ic-diplomes-formation-2012-IDF.csv"
  val outFileName = s"${path}/generated-population-IDF.shp"

  val specs = "geom:Point,age:Integer,sex:Integer,education:Integer"
  val factory = new ShapefileDataStoreFactory
  val dataStore = factory.createDataStore(new File(outFileName).toURI().toURL())
  val featureTypeName = "Object"
  val featureType = DataUtilities.createType(featureTypeName, specs)
  dataStore.createSchema(featureType)
  val typeName = dataStore.getTypeNames()(0)
  val writer = dataStore.getFeatureWriterAppend(typeName, Transaction.AUTO_COMMIT)
  val rng = new Random(42)
  for {
    geom <- reader.readGeometry(new File(contourIRISFileName))
    ageSex <- reader.readAgeSex(new File(baseICEvolStructPopFileName))
    educationSex <- reader.readEducationSex(new File(baseICDiplomesFormationPopFileName))
  } yield {
    reader.generatePopulation(rng, geom, ageSex, educationSex).toIterator.flatten.zipWithIndex.foreach {
      case ((age, sex, education, point), i) =>
        val values = Array[AnyRef](point, age.asInstanceOf[AnyRef], sex.asInstanceOf[AnyRef], education.asInstanceOf[AnyRef])
        val simpleFeature = writer.next()
        simpleFeature.setAttributes(values)
        writer.write()
    }
    writer.close
  }
}
