/**
  * Created by Romain Reuillon on 10/05/16.
  *
  * This program is free software: you can redistribute it and/or modify
  * it under the terms of the GNU Affero General Public License as published by
  * the Free Software Foundation, either version 3 of the License, or
  * (at your option) any later version.
  *
  * This program is distributed in the hope that it will be useful,
  * but WITHOUT ANY WARRANTY; without even the implied warranty of
  * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  * GNU General Public License for more details.
  *
  * You should have received a copy of the GNU General Public License
  * along with this program.  If not, see <http://www.gnu.org/licenses/>.
  *
  */
package eighties

import com.vividsolutions.jts.geom.Point
import monocle.macros.Lenses
import org.geotools.geometry.jts.JTS
import org.geotools.referencing.CRS

object space {
  type Coordinate = (Double, Double)
  type Location = (Int, Int)

  /* définition d'un voisinage*/
  def neighbours(side: Int, location: Location, size: Int) = {
    val (i, j) = location

    for {
      di <- (-size to size)
      dj <- (-size to size)
      if (di != 0 && dj != 0)
      ni = i + di
      nj = j + dj
      if (ni >= 0 && nj >= 0 && ni < side && nj < side)
    } yield (i + di, j + dj)
  }

  def project(p: Point, minX: Int, minY: Int) = {
    val inCRS = CRS.decode("EPSG:2154")
    val outCRS = CRS.decode("EPSG:3035")
    val transform = CRS.findMathTransform(inCRS, outCRS, true)
    def discrete(v:Double) = (v / 200.0).toInt * 200
    val transformedPoint = JTS.transform(p, transform)
    (discrete(transformedPoint.getCoordinate.x) - minX,
      discrete(transformedPoint.getCoordinate.y) - minY)
  }

  /* Définition d'une classe Grid, composé de vecteurs, de edges et de side*/
  @Lenses case class Grid(cells: Vector[Vector[Cell]], side: Int)
  @Lenses case class Cell(location: Location)

}
