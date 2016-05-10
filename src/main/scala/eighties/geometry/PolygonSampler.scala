package eighties.geometry

import com.vividsolutions.jts.geom.{Coordinate, GeometryCollection, Polygon}
import com.vividsolutions.jts.triangulate.ConformingDelaunayTriangulationBuilder

import scala.util.Random

class PolygonSampler(val polygon: Polygon, val tolerance: Double = 0.1) {
  val triangles = {
    val builder = new ConformingDelaunayTriangulationBuilder
    builder.setSites(polygon)
    builder.setConstraints(polygon)
    builder.setTolerance(tolerance)
    val triangleCollection = builder.getTriangles(polygon.getFactory()).asInstanceOf[GeometryCollection]
    var areaSum = 0.0
    val trianglesInPolygon = (0 until triangleCollection.getNumGeometries).map(triangleCollection.getGeometryN(_).asInstanceOf[Polygon]).filter(p => {
      val area = p.getArea
      p.intersection(polygon).getArea() > 0.99 * area
    })
    trianglesInPolygon.map { triangle =>
      areaSum += triangle.getArea
      (areaSum, triangle)
    }
  }
  val totalArea = triangles.last._1
  def apply(rnd: Random) = {
      val s = rnd.nextDouble() * totalArea
      val t = rnd.nextDouble()
      val triangleIndex = triangles.indexWhere(s < _._1)
      val area = triangles(triangleIndex)._1
      val previousArea = if (triangles.isDefinedAt(triangleIndex - 1)) triangles(triangleIndex - 1)._1 else 0.0
      val triangle = triangles(triangleIndex)._2
      val tmp = Math.sqrt((s - previousArea) / (area - previousArea))
      val a = 1 - tmp
      val b = (1 - t) * tmp
      val c = t * tmp
      val coord = triangle.getCoordinates
      val p1 = coord(0)
      val p2 = coord(1)
      val p3 = coord(2)
      val x1 = p1.x
      val x2 = p2.x
      val x3 = p3.x
      val y1 = p1.y
      val y2 = p2.y
      val y3 = p3.y
      val x = a * x1 + b * x2 + c * x3
      val y = a * y1 + b * y2 + c * y3
      new Coordinate(x,y)
  }
}