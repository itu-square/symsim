package symsim
package examples.concrete.windygrid

import examples.concrete.windygrid.GridAction.*
import cats.{Eq, Foldable, Monad}
import cats.kernel.BoundedEnumerable

import org.scalacheck.{Arbitrary, Gen}

import symsim.concrete.Randomized

case class GridState (x: Int, y: Int):
  override def toString: String = s"($x,$y)"
  
type GridObservableState = GridState
type GridReward = Double

enum GridAction: 
  case R, L, U, D


object WindyGrid extends 
  Agent[GridState, GridObservableState, GridAction, GridReward, Randomized]:

  val WindSpec = Array (0, 0, 0, 1, 1, 1, 2, 2, 1, 0)

  def isFinal (s: GridState): Boolean =
    (s.x, s.y) == (8, 4)

  def observe (s: GridState): GridObservableState =
    GridState (s.x, s.y)

  def gridReward (s: GridState) (a: GridAction): GridReward = s match
    case GridState (8, 4) => 0.0   // final state
    case GridState (_, _) => -1.0

  def stepRight (s: GridState): GridState =
    val x1 = (s.x + 1).min (10)
    val y1 = (s.y+WindSpec(s.x-1)).min (7)
    GridState (x1, y1)

  def stepLeft (s: GridState): GridState =
    val x1 = (s.x - 1).max (1)
    val y1 = (s.y + WindSpec (s.x - 1)).min (7)
    GridState (x1, y1)

  def stepUp (s: GridState): GridState =
    val x1 = s.x
    val y1 = (s.y + 1 + WindSpec (s.x - 1)).min (7)
    GridState (x1, y1)

  def stepDown (s: GridState): GridState =
    val x1 = s.x
    val y1 = ((s.y - 1 + WindSpec (s.x - 1)).max (1)).min (7)
    GridState (x1, y1)

  def step (s: GridState) (a: GridAction): Randomized[(GridState, GridReward)] =
    val s1 = a match
      case GridAction.R => stepRight (s)
      case GridAction.L => stepLeft (s)
      case GridAction.U => stepUp (s)
      case GridAction.D => stepDown (s)
    Randomized.const (s1 -> gridReward (s1) (a))

  def initialize: Randomized[GridState] = for
    x <- Randomized.repeat (Randomized.between (1, 11))
    y <- Randomized.repeat (Randomized.between (1, 8))
    s = GridState (x, y) if !isFinal (s)
  yield s

  val instances = WindyGridInstances

end WindyGrid

/** Here is a proof that our types actually deliver on everything that an Agent
  * needs to be able to do to work in the framework.
  */
object WindyGridInstances
  extends AgentConstraints[GridState, GridObservableState, GridAction, 
    GridReward, Randomized]:

  given enumAction: BoundedEnumerable[GridAction] =
    BoundedEnumerableFromList (U, L, R, D)

  given enumState: BoundedEnumerable[GridObservableState] =
    val ss = for
      x <- Seq (1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
      y <- Seq (1, 2, 3, 4, 5, 6, 7)
    yield GridState (x, y)
    BoundedEnumerableFromList (ss*)

  given schedulerIsMonad: Monad[Randomized] = Randomized.randomizedIsMonad

  val schedulerIsFoldable: Foldable[Randomized] = Randomized.randomizedIsFoldable

  given canTestInScheduler: CanTestIn[Randomized] = Randomized.canTestInRandomized

  lazy val genGridState: Gen[GridState] = for
    x <- Gen.choose (1, 10)
    y <- Gen.choose (1, 7)
  yield GridState (x, y)

  given arbitraryState: Arbitrary[GridState] = Arbitrary (genGridState)

  given eqGridState: Eq[GridState] = Eq.fromUniversalEquals

  given arbitraryReward: Arbitrary[GridReward] = Arbitrary (Gen.double)

  given rewardArith: Arith[GridReward] = Arith.arithDouble

end WindyGridInstances
