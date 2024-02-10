package symsim
package examples.concrete.continuousmaze

import cats.{Eq, Foldable, Monad}
import cats.kernel.BoundedEnumerable

import org.scalacheck.{Arbitrary, Gen}

import symsim.concrete.Randomized

/**
  * the agent not only chooses its movement direction but
  * also has the flexibility to determine the size of each step it takes.
  *
  * Accordingly, the states within this setting exhibit a semi-continuous nature,
  * wherein their characteristics may vary depending on the chosen step size.
  *
  * If the agent bumps into a wall, it stays in the same square.
  *
 */

case class MazeState (x: Double, y: Double):
  override def toString: String = s"[x=$x, y=$y]"

type MazeObservableState = MazeState
type MazeReward = Double

enum Direction:
  case R, L, U, D

enum StepSize:
  case B, S

type MazeAction = (Direction, StepSize)

val MazeLength: Int = 4
val MazeWidth: Int = 3

val BigStep: Double = 1
val SmallStep: Double = 0.5

object ContinuousMaze
  extends 
    Agent[MazeState, MazeObservableState, MazeAction, MazeReward, Randomized],
    Episodic:

  val TimeHorizon: Int = 2000

  def isFinal (s: MazeState): Boolean =
    s == MazeState (MazeLength, MazeWidth) || s == MazeState (MazeLength, MazeWidth - 1)

  def observe (s: MazeState): MazeObservableState = MazeState (s.x.floor, s.y.floor)

  private def mazeReward (s: MazeState): MazeReward = s match
    case MazeState (MazeLength, MazeWidth) => +0.0     // Good final state
    case MazeState (MazeLength, y) if y == MazeWidth - 1 => -1000.0   // Bad final state (dead)
    case MazeState (_, _) => -1.0


  def distort (a: MazeAction): Randomized[MazeAction] = a match
    case (Direction.U, a._2) | (Direction.D, a._2) =>
      Randomized.oneOf ((Direction.R, a._2), (Direction.L, a._2))
    case (Direction.L, a._2) | (Direction.R, a._2) =>
      Randomized.oneOf((Direction.U, a._2), (Direction.D, a._2))


  def successor (s: MazeState) (a: MazeAction): MazeState =
    require (valid (s))

    val result = a match
      case (Direction.U, StepSize.B) => MazeState (s.x, s.y + BigStep)
      case (Direction.U, StepSize.S) => MazeState (s.x, s.y + SmallStep)
      case (Direction.D, StepSize.B) => MazeState (s.x, s.y - BigStep)
      case (Direction.D, StepSize.S) => MazeState (s.x, s.y - SmallStep)
      case (Direction.R, StepSize.B) => MazeState (s.x + BigStep, s.y)
      case (Direction.R, StepSize.S) => MazeState (s.x + SmallStep, s.y)
      case (Direction.L, StepSize.B) => MazeState (s.x - BigStep, s.y)
      case (Direction.L, StepSize.S) => MazeState (s.x - SmallStep, s.y)
    if valid (result) then result else s

  def valid (s: MazeState): Boolean =
     s.x >= 1 && s.x <= 4 && s.y >= 1 && s.y <= 3 && s != MazeState (2, 2)

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
      BoundedEnumerableFromList ((Direction.U, StepSize.B), (Direction.U, StepSize.S),
        (Direction.D, StepSize.B), (Direction.D, StepSize.S),
        (Direction.R, StepSize.B), (Direction.R, StepSize.S),
        (Direction.L, StepSize.B), (Direction.L, StepSize.S))

   given enumState: BoundedEnumerable[MazeObservableState] =
      val ss = for
         y <- (1 to MazeWidth).toSeq
         x <- (1 to MazeLength).toSeq
         result = MazeState (x, y)
         if ContinuousMaze.valid (result)
      yield result
      BoundedEnumerableFromList (ss*)

   given schedulerIsMonad: Monad[Randomized] = Randomized.randomizedIsMonad

   given schedulerIsFoldable: Foldable[Randomized] = Randomized.randomizedIsFoldable

   given canTestInScheduler: CanTestIn[Randomized] = Randomized.canTestInRandomized

   lazy val genMazeState: Gen[MazeState] = for
      y <- Gen.choose[Double](1, MazeWidth)
      x <- Gen.choose[Double](1, MazeLength)
      if (x != 2 && y != 2)
   yield MazeState (x.abs, y.abs)

   given arbitraryState: Arbitrary[MazeState] = Arbitrary (genMazeState)

   given eqMazeState: Eq[MazeState] = Eq.fromUniversalEquals

   given arbitraryReward: Arbitrary[MazeReward] = Arbitrary (Gen.double)

   given rewardArith: Arith[MazeReward] = Arith.arithDouble

end ContinuousMazeInstances
