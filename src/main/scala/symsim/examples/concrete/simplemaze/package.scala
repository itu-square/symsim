package symsim.examples.concrete

package object simplemaze:

  /**
   * Russell, Norvig, Fig 17.1, p. 646
   * We map a finite state space with the following maze states
   *
   *   { (1,3), (2,3), (3,3), (4,3),
   *     (1,2),        (3,2), (4,2),
   *     (1,1), (2,1), (3,1), (4,1) } .
   *
   */

  case class MazeState (x: Int, y: Int):
    override def toString: String = s"[x=$x, y=$y]"

  type MazeFiniteState = MazeState
  type MazeReward = Double

  sealed trait MazeAction
  case object Left extends MazeAction
  case object Right extends MazeAction
  case object Up extends MazeAction
  case object Down extends MazeAction
