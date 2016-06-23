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
  def mapColorRGB(world: space.World, file: File,
                  geValue: (Individual=>Double) = i=>i.behaviour,
                  minValue: Double = -0.5,
                  maxValue: Double = 0.5,
                  cellSize: Int = 200, crs: CoordinateReferenceSystem = CRS.decode("EPSG:3035")) = {
    val minX = world.originI
    val minY = world.originJ
    val width = world.sideI
    val height = world.sideJ
    val maxX = minX + width
    val maxY = minY + height
    val rangeValues = maxValue - minValue
    val pixelSize = 10
    val colors = Vector(
      (127.0,0.0,0.0),//dark red
      (255.0,0.0,0.0),//red
      (255.0,127.0,0.0),//orange
      (255.0,255.0,0.0),//yellow
      (127.0,255.0,0.0),//yellow-green
      (0.0,255.0,0.0),//green
      (0.0,255.0,255.0),//green-blue
      (0.0,255.0,255.0),//cyan
      (0.0,127.0,255.0),//cyan-blue
      (0.0,0.0,255.0)//blue
    )
    val bufferedImage = new BufferedImage(width*pixelSize, height*pixelSize, BufferedImage.TYPE_INT_RGB)
    val raster = bufferedImage.getRaster
    val index = space.Index.indexIndividuals(world)
    def interpolate(lambda: Double, c1: (Double, Double, Double), c2: (Double, Double, Double)) = {
      Array(
        (1.0 - lambda) * c1._1 + lambda * c2._1,
        (1.0 - lambda) * c1._2 + lambda * c2._2,
        (1.0 - lambda) * c1._3 + lambda * c2._3)
    }
    def clamp(v: Double) = math.min(math.max(v, 0.0),1.0)
    for {
      (l, i) <- index.cells.zipWithIndex
      (c, j) <- l.zipWithIndex
    } yield {
      val ii = i* pixelSize
      val jj = (height - j - 1) * pixelSize
      val values = c map geValue
      val size = values.size
      def meanValue = values.sum / size
      if (size > 0) {
        val value = clamp((meanValue  - minValue)/ rangeValues) * (colors.size - 1)
        val ind = value.toInt
        val lambda = value - ind
        val c0 = colors(ind)
        val color0 = Array(c0._1,c0._2,c0._3)
        val color = if (ind == colors.size - 1) color0 else interpolate(lambda, colors(ind), colors(ind+1))
        val vec = Array.fill(pixelSize * pixelSize)(color).flatten
        raster.setPixels(ii, jj, pixelSize, pixelSize, vec)
      }
    }
    val referencedEnvelope = new ReferencedEnvelope(minX * cellSize, maxX * cellSize, minY * cellSize, maxY * cellSize, crs)
    val factory = new GridCoverageFactory
    val coverage = factory.create("GridCoverage", bufferedImage, referencedEnvelope)
    format.getWriter(file.toJava).write(coverage, null)
  }
}
