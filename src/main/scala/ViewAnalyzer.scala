import scala.collection.mutable.ListBuffer

/**
 * Created by wayneyu on 11/26/14.
 */

trait ViewAnalyzer{
  val directionValue: Array[Double]
  val resourceValue: Double
  val entityXYs: Map[Char,List[XY]]
  def S = entityXYs.getOrElse('S', Nil)
  def m = entityXYs.getOrElse('m', Nil)
  def s = entityXYs.getOrElse('s', Nil)
  def p = entityXYs.getOrElse('p', Nil)
  def P = entityXYs.getOrElse('P', Nil)
  def b = entityXYs.getOrElse('b', Nil)
  def B = entityXYs.getOrElse('B', Nil)
  def W = entityXYs.getOrElse('W', Nil)
}

case class viewAnalyzerSlave(view: View) extends ViewAnalyzer{

  val (directionValue, resourceValue, entityXYs) = analyzeViewAsSlave(view)

  /** Analyze the view, building a map of attractiveness for the 45-degree directions and
    * recording other relevant data, such as the nearest elements of various kinds.
    */
  def analyzeViewAsSlave(view: View) = {
    val directionValue = Array.ofDim[Double](8)
    var resourceValue = 0.0
    var entityXYs = Map[Char, scala.collection.mutable.ListBuffer[XY]]()
    val cells = view.cells
    val cellCount = cells.length

    for(i <- 0 until cellCount) {
      val cellRelPos = view.relPosFromIndex(i)
      if(cellRelPos.isNonZero) {
        cells(i) match {
          case '-' =>
          case c: Char =>
            entityXYs.get(c) match {
              case Some(l) => l += cellRelPos
              case None => entityXYs += (c -> ListBuffer[XY](cellRelPos))
            }
        }
        val stepDistance = cellRelPos.stepCount
        val value: Double = cells(i) match {
          case 'm' => // another master: causes EU lost
            if(stepDistance < 2) -1000 else 0

          case 's' => // another slave: causes EU lost
            if(stepDistance < 2) -1000 else 0

          case 'S' => // our own slave
            -100 // to spread out mini-bots

          case 'B' => // good beast: valuable, but runs away
            resourceValue += 200/stepDistance
            if(stepDistance == 1) 600
            else if(stepDistance == 2) 400
            else (430 - stepDistance * 15).max(10)

          case 'P' => // good plant: less valuable, but does not run
            resourceValue += 100/stepDistance
            if(stepDistance == 1) 500
            else if(stepDistance == 2) 300
            else (320 - stepDistance * 10).max(10)

          case 'b' => // bad beast: dangerous, but only if very close
            if(stepDistance < 4) -400 / stepDistance else -50 / stepDistance

          case 'p' => // bad plant: bad, but only if I step on it
            if(stepDistance < 2) -1000 else 0

          case 'W' => // wall: harmless, just don't walk into it
            if(stepDistance < 2) -1000
            else if (stepDistance < 5) -10
            else 0

          case _ => 0.0
        }
        val direction45 = cellRelPos.toDirection45
        directionValue(direction45) += value
      }
    }
    (directionValue, resourceValue, entityXYs.map{ case (c, bl) => (c, bl.toList.sortBy(_.length)) })
  }

}

case class viewAnalyzerMaster(view: View) extends ViewAnalyzer {

  val (directionValue, resourceValue, entityXYs) = analyzeViewAsMaster(view)

  /** Analyze the view, building a map of attractiveness for the 45-degree directions and
    * recording other relevant data, such as the nearest elements of various kinds.
    */
  def analyzeViewAsMaster(view: View) = {
    val directionValue = Array.ofDim[Double](8)
    var resourceValue = 0.0
    var entityXYs = Map[Char, ListBuffer[XY]]()

    val cells = view.cells
    val cellCount = cells.length
    for(i <- 0 until cellCount) {
      val cellRelPos = view.relPosFromIndex(i)
      if(cellRelPos.isNonZero) {
        cells(i) match {
          case '-' =>
          case c: Char =>
            entityXYs.get(c) match {
              case Some(l) => l += cellRelPos
              case None => entityXYs += (c -> ListBuffer[XY](cellRelPos))
            }
        }
        val stepDistance = cellRelPos.stepCount
        val value: Double = cells(i) match {
          case 'm' => // another master: not dangerous, but an obstacle
            if(stepDistance < 2) -1000 else 0

          case 's' => // another slave: valuable but can be dangerous, rather avoid
            if(stepDistance < 3) -1000 else 0

          case 'S' => // our own slave
            10

          case 'B' => // good beast: valuable, but runs away
            resourceValue += 200
            if(stepDistance == 1) 600
            else if(stepDistance == 2) 400
            else (430 - stepDistance * 15).max(10)

          case 'P' => // good plant: less valuable, but does not run
            resourceValue += 100
            if(stepDistance == 1) 500
            else if(stepDistance == 2) 300
            else (320 - stepDistance * 10).max(10)

          case 'b' => // bad beast: dangerous, but only if very close
            if(stepDistance < 4) -400 / stepDistance else -50 / stepDistance

          case 'p' => // bad plant: bad, but only if I step on it
            if(stepDistance < 2) -1000 else 0

          case 'W' => // wall: harmless, just don't walk into it
            if(stepDistance < 2) -1000
            else if (stepDistance < 5) -10
            else 0

          case _ => 0.0
        }
        val direction45 = cellRelPos.toDirection45
        directionValue(direction45) += value

      }
    }
    (directionValue, resourceValue, entityXYs.map{ case (c, bl) => (c, bl.toList) })
  }

}
