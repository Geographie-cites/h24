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
  def cat(ind: Individual) = AggregatedEducation(ind.education) match {
    case Some(AggregatedEducation.Low) => 0
    case Some(AggregatedEducation.Middle) => 1
    case Some(AggregatedEducation.High) => 2
    case None => -1
  }
  def mapRGB(world: space.World, file: File,
             geValue: (Individual=>Int) = cat,
             cellSize: Int = 200, crs: CoordinateReferenceSystem = CRS.decode("EPSG:3035")) = {
    val minX = world.originI
    val minY = world.originJ
    val width = world.sideI
    val height = world.sideJ
    val maxX = minX + width
    val maxY = minY + height
    val bufferedImage = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB)
    val raster = bufferedImage.getRaster
    val index = space.Index.indexIndividuals(world)

    for {
      (l, i) <- index.cells.zipWithIndex
      (c, j) <- l.zipWithIndex
    } yield {
      val jj = height - j - 1
      val value = c map geValue
      val size = value.size
      val vec = if (size == 0) Array(0,0,0)
      else Array(value.count(_ == 0)*255/size, value.count(_ == 1)*255/size, value.count(_ == 2)*255/size)
      raster.setPixel(i, jj, vec)
    }
    val referencedEnvelope = new ReferencedEnvelope(minX * cellSize, maxX * cellSize, minY * cellSize, maxY * cellSize, crs)
    val factory = new GridCoverageFactory
    val coverage = factory.create("GridCoverage", bufferedImage, referencedEnvelope)
    format.getWriter(file.toJava).write(coverage, null)
  }
  def mapGray(world: space.World, file: File,
              geValue: (Individual=>Double) = i=>i.behaviour,
              cellSize: Int = 200, crs: CoordinateReferenceSystem = CRS.decode("EPSG:3035")) = {
    val minX = world.originI
    val minY = world.originJ
    val width = world.sideI
    val height = world.sideJ
    val maxX = minX + width
    val maxY = minY + height
    val pixelSize = 10
    val bufferedImage = new BufferedImage(width*pixelSize, height*pixelSize, BufferedImage.TYPE_USHORT_GRAY)
    val raster = bufferedImage.getRaster
    val index = space.Index.indexIndividuals(world)

    val maxValue = Math.pow(2,16) - 1.0
    for {
      (l, i) <- index.cells.zipWithIndex
      (c, j) <- l.zipWithIndex
    } yield {
      val ii = i* pixelSize
      val jj = (height - j - 1) * pixelSize
      val value = c map geValue
      val size = value.size
      val vec = if (size == 0) Array.fill(pixelSize * pixelSize)(0) else Array.fill(pixelSize * pixelSize)((value.sum*maxValue/size).toInt)
      raster.setPixels(ii, jj, pixelSize, pixelSize, vec)
    }
    val referencedEnvelope = new ReferencedEnvelope(minX * cellSize, maxX * cellSize, minY * cellSize, maxY * cellSize, crs)
    val factory = new GridCoverageFactory
    val coverage = factory.create("GridCoverage", bufferedImage, referencedEnvelope)
    format.getWriter(file.toJava).write(coverage, null)
  }
}
