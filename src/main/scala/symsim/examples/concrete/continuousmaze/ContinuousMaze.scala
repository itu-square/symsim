package symsim
package examples.concrete.continuousmaze

import cats.{Eq, Foldable, Monad}
import cats.kernel.BoundedEnumerable

import org.scalacheck.{Arbitrary, Gen}

import symsim.concrete.Randomized

/**
 *
 */

case class MazeState (x: Double, y: Double):
  override def toString: String = s"[x=$x, y=$y]"

type MazeObservableState = MazeState
type MazeReward = Double

enum Direction:
  case R, L, U, D

enum Velocity:
  case B, S

type MazeAction = (Direction, Velocity)

val MAZE_LENGTH: Int = 4
val MAZE_WIDTH: Int = 3

val BIG_STEP: Double = 1
val SMALL_STEP: Double = 0.5

object ContinuousMaze
  extends 
    Agent[MazeState, MazeObservableState, MazeAction, MazeReward, Randomized],
    Episodic:

  val TimeHorizon: Int = 2000

  def isFinal (s: MazeState): Boolean =
    s == MazeState (MAZE_LENGTH, MAZE_WIDTH) || s == MazeState (MAZE_LENGTH, MAZE_WIDTH - 1)

  // Maze is discrete
  def observe (s: MazeState): MazeObservableState = MazeState (s.x.floor, s.y.floor)

  // We are not using the original reward function from AIAMA as it
  // gives to unstable learning results
  private def mazeReward (s: MazeState): MazeReward = s match
    case MazeState (MAZE_LENGTH, MAZE_WIDTH) => +0.0     // Good final state
    case MazeState (MAZE_LENGTH, y) if y == MAZE_WIDTH - 1 => -1000.0   // Bad final state (dead)
    case MazeState (_, _) => -1.0


  def distort (a: MazeAction): Randomized[MazeAction] = a match
    case (Direction.U, a._2) | (Direction.D, a._2) =>
      Randomized.oneOf ((Direction.R, a._2), (Direction.L, a._2))
    case (Direction.L, a._2) | (Direction.R, a._2) =>
      Randomized.oneOf((Direction.U, a._2), (Direction.D, a._2))


  def successor (s: MazeState) (a: MazeAction): MazeState =
    require (valid (s))

    val result = a match
      case (Direction.U, Velocity.B) => MazeState (s._1, s._2 + BIG_STEP)
      case (Direction.U, Velocity.S) => MazeState (s._1, s._2 + SMALL_STEP)
      case (Direction.D, Velocity.B) => MazeState (s._1, s._2 - BIG_STEP)
      case (Direction.D, Velocity.S) => MazeState (s._1, s._2 - SMALL_STEP)
      case (Direction.R, Velocity.B) => MazeState (s._1 + BIG_STEP, s._2)
      case (Direction.R, Velocity.S) => MazeState (s._1 + SMALL_STEP, s._2)
      case (Direction.L, Velocity.B) => MazeState (s._1 - BIG_STEP, s._2)
      case (Direction.L, Velocity.S) => MazeState (s._1 - SMALL_STEP, s._2)
    if valid (result) then result else s

  def valid (s: MazeState): Boolean =
     s._1 >= 1 && s._1 <= 4 && s._2 >= 1 && s._2 <= 3 && s != MazeState (2, 2)

  val attention = 0.8

  def step (s: MazeState) (a: MazeAction): Randomized[(MazeState, MazeReward)] =
    for
      precise <- Randomized.coin (attention)
      action <- if precise then Randomized.const (a) else distort (a)
      newState = successor (s) (action)
    yield (newState, mazeReward (newState))

  def initialize: Randomized[MazeState] =
    Randomized.repeat (Randomized.oneOf (instances.allObservableStates*))
      .filter (s => !isFinal (s))

  val instances = ContinuousMazeInstances

end ContinuousMaze


/** Here is a proof that our types actually deliver on everything that an Agent
  * needs to be able to do to work in the framework.
  */
object ContinuousMazeInstances
   extends AgentConstraints[MazeState, MazeObservableState, MazeAction, MazeReward, Randomized]:

   given enumAction: BoundedEnumerable[MazeAction] =
      BoundedEnumerableFromList ((Direction.U, Velocity.B), (Direction.U, Velocity.S),
        (Direction.D, Velocity.B), (Direction.D, Velocity.S),
        (Direction.R, Velocity.B), (Direction.R, Velocity.S),
        (Direction.L, Velocity.B), (Direction.L, Velocity.S))

   given enumState: BoundedEnumerable[MazeObservableState] =
      val ss = for
         y <- (1 to MAZE_WIDTH).toSeq
         x <- (1 to MAZE_LENGTH).toSeq
         result = MazeState (x, y)
         if ContinuousMaze.valid (result)
      yield result
      BoundedEnumerableFromList (ss*)

   given schedulerIsMonad: Monad[Randomized] = Randomized.randomizedIsMonad

   given schedulerIsFoldable: Foldable[Randomized] = Randomized.randomizedIsFoldable

   given canTestInScheduler: CanTestIn[Randomized] = Randomized.canTestInRandomized

   lazy val genMazeState: Gen[MazeState] = for
      y <- Gen.choose[Double](1, MAZE_WIDTH)
      x <- Gen.choose[Double](1, MAZE_LENGTH)
      if (x != 2 && y != 2)
   yield MazeState (x.abs, y.abs)

   given arbitraryState: Arbitrary[MazeState] = Arbitrary (genMazeState)

   given eqMazeState: Eq[MazeState] = Eq.fromUniversalEquals

   given arbitraryReward: Arbitrary[MazeReward] = Arbitrary (Gen.double)

   given rewardArith: Arith[MazeReward] = Arith.arithDouble

end ContinuousMazeInstances
