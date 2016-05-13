package eighties

import java.awt.image.BufferedImage

import better.files.File
import eighties.population.{AggregatedEducation, Individual}
import org.geotools.coverage.grid.GridCoverageFactory
import org.geotools.gce.geotiff.GeoTiffFormat
import org.geotools.geometry.jts.ReferencedEnvelope
import org.geotools.referencing.CRS
import org.opengis.referencing.crs.CoordinateReferenceSystem

object WorldMapper {
  val format = new GeoTiffFormat()
  def map(world: space.World, file: File, cellSize: Int = 200, crs: CoordinateReferenceSystem = CRS.decode("EPSG:3035")) = {
    val minX = world.originI
    val minY = world.originJ
    val width = world.sideI
    val height = world.sideJ
    val maxX = minX + width
    val maxY = minY + height
    val bufferedImage = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB)
    val raster = bufferedImage.getRaster
    val index = space.Index.indexIndividuals(world)

    def cat(ind: Individual) = AggregatedEducation(ind.education) match {
      case Some(AggregatedEducation.Low) => 0
      case Some(AggregatedEducation.Middle) => 1
      case Some(AggregatedEducation.High) => 2
      case None => -1
    }

    for {
      (l, i) <- index.cells.zipWithIndex
      (c, j) <- l.zipWithIndex
    } yield {
      val jj = height - j - 1
      val education = c map cat
      val size = education.size
      val vec = if (size == 0) Array(0,0,0)
      else Array(education.count(_ == 0)*255/size, education.count(_ == 1)*255/size, education.count(_ == 2)*255/size)
      raster.setPixel(i, jj, vec)
    }
    val referencedEnvelope = new ReferencedEnvelope(minX * cellSize, maxX * cellSize, minY * cellSize, maxY * cellSize, crs)
    val factory = new GridCoverageFactory
    val coverage = factory.create("GridCoverage", bufferedImage, referencedEnvelope)
    format.getWriter(file.toJava).write(coverage, null)
  }
}
