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
import org.joda.time.format.DateTimeFormat

/**
  */
object EGTTestMatrixWrite extends App {
  val path = File("../données/EGT 2010/presence semaine EGT")
  val outputPath = File("results")
  outputPath.createDirectories()
  val outFileRes = outputPath / "matrix"
  generation.flowsFromEGT(path / "presence_semaine_GLeRoux.csv.lzma",outFileRes)
}
