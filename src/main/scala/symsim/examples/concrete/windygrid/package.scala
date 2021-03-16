package symsim.examples.concrete

package object windygrid {

  /**
   * We map the grid --10x7-- states to
   *
   *   { (0,0), (1,0),...,(10,7)}.
   *
   */

  case class GridState (x: Int, y: Int)
  type GridFiniteState = GridState
  type GridReward = Double
  object GridAction extends Enumeration {
  type GridAction = Value
  val R,L,U,D = Value
  }

}
