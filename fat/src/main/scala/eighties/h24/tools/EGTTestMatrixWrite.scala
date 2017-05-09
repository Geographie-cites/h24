package eighties.h24.tools

import java.text.SimpleDateFormat

import better.files.File
import com.vividsolutions.jts.geom.{Coordinate, GeometryFactory}
import eighties.h24.generation.Flow
import eighties.h24.{generation, space}
import org.geotools.data.shapefile.ShapefileDataStoreFactory
import org.geotools.data.{DataUtilities, Transaction}
import org.geotools.geometry.jts.JTS
import org.geotools.referencing.CRS
import org.joda.time.{DateTime, Interval}
import org.joda.time.format.DateTimeFormat

/**
  */
object EGTTestMatrixWrite extends App {
  val path = File("../données/EGT 2010/presence semaine EGT")
  val outputPath = File("results")
  outputPath.createDirectories()
  val outFileRes = outputPath / "matrix"
  val intervals = Vector(
    new Interval(new DateTime(2010,1,1,0,0), new DateTime(2010,1,1,6,0)),
    new Interval(new DateTime(2010,1,1,6,0), new DateTime(2010,1,1,12,0)),
    new Interval(new DateTime(2010,1,1,12,0), new DateTime(2010,1,1,18,0)),
    new Interval(new DateTime(2010,1,1,18,0), new DateTime(2010,1,2,0,0))
  )
  generation.flowsFromEGT(path / "presence_semaine_GLeRoux.csv.lzma",intervals,outFileRes)
}
