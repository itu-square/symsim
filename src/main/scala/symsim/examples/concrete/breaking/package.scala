package symsim.examples.concrete

package object breaking {

  /**
   * We map the car states to
   *
   *   { (0,5], (5,10], (10,15], (15,infty) } X { [0,5), [5,10), (10, infty) }.
   *
   * We represent these states by the left point of the interval, so we can use
   * the same type to represent the finite state space.
   */

  case class CarState (v: Double, p: Double)
  type CarFiniteState = CarState
  type CarAction = Double
  type CarReward = Double

}
