package eighties

import better.files.File
import com.vividsolutions.jts.geom.Point
import eighties.space.{Attraction, World}
import org.geotools.data.shapefile.ShapefileDataStore
import org.geotools.gce.geotiff.GeoTiffFormat

import scala.util.{Random, Try}

object MapPopulation extends App {
  def readPopulation(aFile: File) = {
    val store = new ShapefileDataStore(aFile.toJava.toURI.toURL)
    val reader = store.getFeatureReader
    val featureReader = Iterator.continually(reader.next).takeWhile(_ => reader.hasNext)
    //val specs = "geom:Point:srid=3035,cellX:Integer,cellY:Integer,ageCat:Integer,age:Double,sex:Integer,education:Integer,work:Point:srid=3035"
    val result =
      Try {
        featureReader
          .map { feature =>
            val geom = feature.getDefaultGeometry.asInstanceOf[Point]
            val x = feature.getAttribute("cellX").toString.toInt
            val y = feature.getAttribute("cellY").toString.toInt
            val ageCat = feature.getAttribute("ageCat").toString.toInt
            val age = feature.getAttribute("age").toString.toDouble
            val sex = feature.getAttribute("sex").toString.toInt
            val education = feature.getAttribute("education").toString.toInt
            (ageCat, age, x, y, sex, education, geom)
          }.toVector
      }
    reader.close
    store.dispose
    result
  }

  val path = File("results")
  val inputFile = path / "generated-population.shp"
  val outFile = path / "generated-population-75-work.tiff"
  // val format = GridFormatFinder.findFormat( outFile.toJava )
  val format = new GeoTiffFormat()
  val random = Random

  for {
    individuals <- readPopulation(inputFile)
  } {
    val features = individuals.map { vec =>
      //(ageCat, age, x, y, sex, education, geom)
      val point = vec._7
      val location = (point.getX, point.getY)
      val home = space.cell(location)
      val age = population.Age(vec._1).get
      val sex = population.Sex(vec._5).get
      val education = population.Education(vec._6).get
      new population.Individual(
        age = age,
        sex = sex,
        education = education,
        Simulation.byEducation(age, sex, education, random),
        home,
        None,
        home
      )
    }
    val world = World(features, Vector[Attraction]())
    WorldMapper.mapRGB(world, File("results") / "test.tiff")
  }
}
