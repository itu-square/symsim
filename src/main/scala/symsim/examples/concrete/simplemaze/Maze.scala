package symsim
package examples.concrete.simplemaze

import cats.{Eq, Monad}
import cats.kernel.BoundedEnumerable

import org.scalacheck.Gen
import org.scalacheck.Arbitrary

import symsim.concrete.Randomized

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


object Maze
  extends Agent[MazeState, MazeFiniteState, MazeAction, MazeReward, Randomized]:

    def isFinal (s: MazeState): Boolean = s.x == 4 && (s.y == 2 || s.y == 3)

    // Maze is discrete
    def discretize (s: MazeState): MazeFiniteState =  s

    private def mazeReward (s: MazeState): MazeReward =
      if s == MazeState(x=4,y=3) then 10          //Good final state
      else if (s == MazeState(x=4,y=2)) then -10  // Bad final state
      else -1

    // TODO: this is now deterministic but eventually needs to be randomized
    def step (s: MazeState) (a: MazeAction): (MazeState, MazeReward) =
      val newstate = a match {
        case Up =>    stepUp(s)
        case Down =>  stepDown(s)
        case Left =>  stepLeft(s)
        case Right => stepRight(s)
      }
      if isValid (newstate)
      then (newstate, mazeReward(newstate))
      else (s, mazeReward(s))


    def  isValid (s: MazeState) =
      s.x >= 1 && s.x <=4 && s.y >= 1 && s.y <= 3 && (s != MazeState (2,2))

    def stepUp(s: MazeState): MazeState = MazeState(x=s.x,y=s.y+1)


    def stepDown(s: MazeState): MazeState = MazeState(x=s.x,y=s.y-1)


    def stepLeft(s: MazeState): MazeState = MazeState(x=s.x-1,y=s.y)


    def stepRight(s: MazeState): MazeState = MazeState(x=s.x+1,y=s.y)


    def initialize: Randomized[MazeState] = for
      y <- Randomized.between (1, 3)
      x <- Randomized.between (1, 4)
      s0 = MazeState (x,y)
      s <- if isFinal (s0) || s0 == MazeState(2,2)
           then initialize
           else Randomized.const (s0)
    yield s

    override def zeroReward: MazeReward = 0

    val instances = MazeInstances



/** Here is a proof that our types actually deliver on everything that an Agent
  * needs to be able to do to work in the framework.
  */
object MazeInstances
  extends AgentConstraints[MazeState, MazeFiniteState, MazeAction, MazeReward, Randomized]:

  given enumAction: BoundedEnumerable[MazeAction] =
    BoundedEnumerableFromList (Up, Down, Left, Right)

  given enumState: BoundedEnumerable[MazeFiniteState] =
    val ss = for
      y <- Seq (1, 2, 3)
      x <- Seq (1, 2, 3, 4)
    yield MazeState (x,y)
    BoundedEnumerableFromList (ss: _*)

  given schedulerIsMonad: Monad[Randomized] =
    concrete.Randomized.randomizedIsMonad

  given canTestInScheduler: CanTestIn[Randomized] =
    concrete.Randomized.canTestInRandomized

  lazy val genMazeState: Gen[MazeState] = for
    y <- Gen.choose (1,3)
    x <- Gen.choose (1,4) if (x != 2 && y != 2)
  yield MazeState (y=Math.abs (y), x=Math.abs (x))

  given arbitraryState: Arbitrary[MazeState] = Arbitrary (genMazeState)

  given eqMazeState: Eq[MazeState] = Eq.fromUniversalEquals

  given arbitraryReward: Arbitrary[MazeReward] = Arbitrary (Gen.double)

  given rewardArith: Arith[MazeReward] = Arith.given_Arith_Double
