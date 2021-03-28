package symsim.examples.concrete

package object simplemaze {

  /**
   * Russell, Norvig, Fig 17.1, p. 646
   * We map a finite state space with the following maze states 
   *
   *   { (3,1), (3,2), (3,3), (3,4),
   *     (2,1),        (2,3), (2,4),
   *     (1,1), (1,2), (1,3), (1,4) } .
   *
   */

  case class MazeState (x: Int, y: Int)
  type MazeFiniteState = MazeState
  // type MazeAction = Int // Can we use an enumeration type { Up, Down, Left, Right } ?
  type MazeReward = Int

  trait MazeAction 
  case object Left extends MazeAction 
  case object Right extends MazeAction 
  case object Up extends MazeAction 
  case object Down extends MazeAction 
  
}
