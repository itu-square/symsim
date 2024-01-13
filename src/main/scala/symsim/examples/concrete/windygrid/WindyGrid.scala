package symsim
package examples.concrete.windygrid

import examples.concrete.windygrid.GridAction.*
import cats.{Eq, Foldable, Monad}
import cats.kernel.BoundedEnumerable
import cats.syntax.all.*

import org.scalacheck.{Arbitrary, Gen}

import symsim.concrete.Randomized2

val xMin = 1
val xMax = 10
val yMin = 1
val yMax = 7

case class GridState (x: Int, y: Int):
  require (x >= 1 && x <= 10 && y >= 1 && y <= 7)
  override def toString: String = s"($x,$y)"

type GridObservableState = GridState
type GridReward = Double

enum GridAction: 
  case R, L, U, D


class WindyGrid (using probula.RNG) extends 
  Agent[GridState, GridObservableState, GridAction, GridReward, Randomized2]:

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

  def step (s: GridState) (a: GridAction): Randomized2[(GridState, GridReward)] =
    val s1 = a match
      case GridAction.R => stepRight (s)
      case GridAction.L => stepLeft (s)
      case GridAction.U => stepUp (s)
      case GridAction.D => stepDown (s)
    Randomized2.const (s1 -> gridReward (s1) (a))

  def initialize: Randomized2[GridState] = { for
    x <- Randomized2.between (1, 11)
    y <- Randomized2.between (1, 8)
    s = GridState (x, y)
  yield s }.filter { !this.isFinal (_) }

  val instances = new WindyGridInstances

end WindyGrid

/** Here is a proof that our types actually deliver on everything that an Agent
  * needs to be able to do to work in the framework.
  */
class WindyGridInstances (using probula.RNG)
  extends AgentConstraints[GridState, GridObservableState, GridAction, 
    GridReward, Randomized2]:

  given enumAction: BoundedEnumerable[GridAction] =
    BoundedEnumerableFromList (U, L, R, D)

  given enumState: BoundedEnumerable[GridObservableState] =
    val ss = for
      x <- xMin to xMax
      y <- yMin to yMax
    yield GridState (x, y)
    BoundedEnumerableFromList (ss*)

  given schedulerIsMonad: Monad[Randomized2] = 
    Randomized2.randomizedIsMonad

  given canTestInScheduler: CanTestIn[Randomized2] = 
    Randomized2.canTestInRandomized

  lazy val genGridState: Gen[GridState] = for
    x <- Gen.choose (1, 10)
    y <- Gen.choose (1, 7)
  yield GridState (x, y)

  given arbitraryState: Arbitrary[GridState] = 
    Arbitrary (genGridState)

  given eqGridState: Eq[GridState] = Eq.fromUniversalEquals

  given arbitraryReward: Arbitrary[GridReward] = 
    Arbitrary (Gen.double)

  given rewardArith: Arith[GridReward] = Arith.arithDouble

end WindyGridInstances
