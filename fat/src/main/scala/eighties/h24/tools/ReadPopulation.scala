package eighties.h24.tools

import java.util.zip.GZIPInputStream

import better.files._
import eighties.h24.generation._
import eighties.h24.population._
import org.tukaani.xz._

object ReadPopulation extends App {
  val res = IndividualFeature.load(File("results/population.csv.gz"))
  println(res.size)
}
