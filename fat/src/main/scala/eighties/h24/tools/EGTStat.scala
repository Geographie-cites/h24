package eighties.h24.tools

import eighties.h24.dynamic.MoveMatrix.Category

object EGTStat extends App {

  import eighties.h24.dynamic._

  val ls = MoveMatrix.moves(Category.all.head) composeLens MoveMatrix.location



}
