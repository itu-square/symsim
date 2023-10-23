package symsim
package examples.concrete.multiAgentMaze

import cats.{Eq, Foldable, Monad}
import cats.kernel.BoundedEnumerable

import org.scalacheck.{Arbitrary, Gen}

import symsim.concrete.Randomized

/**
 * TODO: add comments later
 */

type Position = (Int, Int)
case class MAMazeState (pos1: Position, pos2: Position)

type MAMazeObservableState = MAMazeState
type MAMazeReward = Double

enum Move:
  case Up, Down, Right, Left

type MAMazeAction = (Move, Move)


object MAMaze
  extends 
    Agent[MAMazeState, MAMazeObservableState, MAMazeAction, MAMazeReward, Randomized],
    Episodic:

  val TimeHorizon: Int = 2000

  def isFinal (s: MAMazeState): Boolean =
    s.pos1 == s.pos2

  // MAMaze is discrete
  def observe (s: MAMazeState): MAMazeObservableState = s


  private def mazeReward (s: MAMazeState): MAMazeReward = s match
    case MAMazeState ((2, 3), (2, 3)) => +0.0     // Good final state
    case MAMazeState (p1, p2) if p1 == p2  => -1000.0   // Bad final state (dead)
    case MAMazeState (_, _) => -1.0

  def distort(m: Move): Randomized[Move] = m match
    case Move.Up | Move.Down => Randomized.oneOf (Move.Left, Move.Right)
    case Move.Left | Move.Right => Randomized.oneOf (Move.Up, Move.Down)

  def successor (s: MAMazeState) (a: MAMazeAction): MAMazeState =
    require (valid (s.pos1))
    require (valid (s.pos2))
    val res1 = a._1 match
      case Move.Up    => (s.pos1._1, s.pos1._2+1)
      case Move.Down  => (s.pos1._1, s.pos1._2-1)
      case Move.Left  => (s.pos1._1-1, s.pos1._2)
      case Move.Right => (s.pos1._1+1, s.pos1._2)
    val pos1 = if valid (res1) then res1 else s.pos1
    val res2 = a._2 match
      case Move.Up => (s.pos2._1, s.pos2._2 + 1)
      case Move.Down => (s.pos2._1, s.pos2._2 - 1)
      case Move.Left => (s.pos2._1 - 1, s.pos2._2)
      case Move.Right => (s.pos2._1 + 1, s.pos2._2)
    val pos2 = if valid (res2) then res2 else s.pos2

    MAMazeState (pos1, pos2)

  def valid (p: Position): Boolean =
    p._1 >= 1 && p._1 <= 3 && p._2 >= 1 && p._2 <= 3

  val attention = 0.8

  def step (s: MAMazeState) (a: MAMazeAction): Randomized[(MAMazeState, MAMazeReward)] = for
    precise <- Randomized.coin (attention)
    action <- if precise then Randomized.const (a) else {
      for
        a1 <- distort (a._1)
        a2 <- distort (a._2)
      yield (a1, a2)
    }
    newState = successor (s) (action)
  yield (newState, mazeReward (newState))

  def initialize: Randomized[MAMazeState] = Randomized.const(MAMazeState((1, 1), (4, 1)))

  val instances = MAMazeInstances

end MAMaze


/** Here is a proof that our types actually deliver on everything that an Agent
  * needs to be able to do to work in the framework.
  */
object MAMazeInstances
   extends AgentConstraints[MAMazeState, MAMazeObservableState, MAMazeAction, MAMazeReward, Randomized]:

   given enumAction: BoundedEnumerable[MAMazeAction] =
     val aa = for
       a1 <- List (Move.Up, Move.Down, Move.Right, Move.Left)
       a2 <- List (Move.Up, Move.Down, Move.Right, Move.Left)
     yield (a1, a2)
     BoundedEnumerableFromList (aa*)

   given enumState: BoundedEnumerable[MAMazeObservableState] =
      val ss = for
         y1 <- List (1, 2, 3)
         x1 <- List (1, 2, 3)
         y2 <- List(1, 2, 3)
         x2 <- List(1, 2, 3)
         pos1 = (x1, y1)
         pos2 = (x2, y2)
         result = MAMazeState (pos1, pos2)
      yield result
      BoundedEnumerableFromList (ss*)

   given schedulerIsMonad: Monad[Randomized] = Randomized.randomizedIsMonad

   given schedulerIsFoldable: Foldable[Randomized] = Randomized.randomizedIsFoldable

   given canTestInScheduler: CanTestIn[Randomized] = Randomized.canTestInRandomized

   lazy val genMAMazeState: Gen[MAMazeState] = for
     y1 <- Gen.choose (1, 3)
     x1 <- Gen.choose (1, 3)
     y2 <- Gen.choose(1, 3)
     x2 <- Gen.choose(1, 3)
   yield MAMazeState((x1.abs, y1.abs), (x2.abs, y2.abs))

   given arbitraryState: Arbitrary[MAMazeState] = Arbitrary (genMAMazeState)

   given eqMAMazeState: Eq[MAMazeState] = Eq.fromUniversalEquals

   given arbitraryReward: Arbitrary[MAMazeReward] = Arbitrary (Gen.double)

   given rewardArith: Arith[MAMazeReward] = Arith.arithDouble

end MAMazeInstances
