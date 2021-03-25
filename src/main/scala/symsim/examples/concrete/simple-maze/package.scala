package symsim.examples.concrete

package object simple-maze {

  /**
   * Russell, Norvig, Fig 17.1, p. 646
   * We map a finite state space with the following maze states 
   *
   *   { (3,1), (3,2), (3,3), (3,4),
   *     (2,1),        (2,3), (2,4),
   *     (1,1), (1,2), (1,3), (1,4) } .
   *
   */

  case class MazeState (v: Double, p: Double)
  type MazeFiniteState = MazeState
  type MazeAction = Double // Can we use an enumeration type { Up, Down, Left, Right } ?
  type MazeReward = Double

}
