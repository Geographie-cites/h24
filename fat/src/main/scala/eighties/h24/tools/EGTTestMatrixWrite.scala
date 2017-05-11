package eighties.h24.tools

import java.text.SimpleDateFormat

import better.files.File
import com.vividsolutions.jts.geom.{Coordinate, GeometryFactory}
import eighties.h24.dynamic.MoveMatrix
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
  val path = File("../data/EGT 2010/presence semaine EGT")
  val outputPath = File("results")
  outputPath.createDirectories()

  val outFileRes = outputPath / "matrix.bin"

  generation.flowsFromEGT(149, 132, path / "presence_semaine_GLeRoux.csv.lzma").foreach {
    newMatrix => MoveMatrix.save(newMatrix, outFileRes)
  }
}
