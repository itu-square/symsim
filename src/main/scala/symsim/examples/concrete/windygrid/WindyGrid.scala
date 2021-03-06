package symsim
package examples.concrete.windygrid

//import symsim.concrete.Randomized
import examples.concrete.windygrid.GridAction._
import cats.{Eq, Monad}
import cats.kernel.BoundedEnumerable

import org.scalacheck.Gen
import org.scalacheck.Arbitrary

import symsim.concrete.Randomized

case class GridState (x: Int, y: Int)
type GridFiniteState = GridState
type GridReward = Double
object GridAction extends Enumeration {
type GridAction = Value
val R,L,U,D = Value
}


object WindyGrid
  extends Agent[GridState, GridFiniteState, GridAction, GridReward, Randomized]:

    val windspec = Array(0,0,0,1,1,1,2,2,1,0)

    def isFinal (s: GridState): Boolean =
      (s.x,s.y) == (8,4)

    def discretize (s: GridState): GridFiniteState =
      GridState (s.x, s.y)



    def GridReward (s: GridState) (a: GridAction): GridReward = s match
      case GridState (8, 4) => 0.0   // final state
      case GridState (_, _) => -1.0


    def stepRight (s: GridState): GridState =
      val x1 = Math.min(s.x+1,10)
      val y1 = Math.min(s.y+windspec(s.x-1),7)
      GridState (x1, y1)


    def stepLeft (s: GridState): GridState =
      val x1 = Math.max(s.x-1,1)
      val y1 = Math.min(s.y+windspec(s.x-1),7)
      GridState (x1, y1)


    def stepUp (s: GridState): GridState =
      val x1 = s.x
      val y1 = Math.min(s.y+1+windspec(s.x-1),7)
      GridState (x1, y1)


    def stepDown (s: GridState): GridState =
      val x1 = s.x
      val y1 = Math.min(Math.max(s.y-1+windspec(s.x-1),1),7)
      GridState (x1, y1)


    /** Granularity of the step in seconds */
    // TODO: this is now deterministic but eventually needs to be randomized
    def step (s: GridState) (a: GridAction): Randomized[(GridState, GridReward)] =
      val s1=if(a==R) stepRight(s)
             else if (a==L) stepLeft(s)
             else if (a==U) stepUp(s)
             else stepDown(s)
      Randomized.const(s1-> GridReward (s1) (a))

    def initialize: Randomized[GridState] =
      for
          x <- Randomized.between(1,11)
          y <- Randomized.between(1,8)
          s0 = GridState (x,y)
          s<- if isFinal (s0) then initialize else Randomized.const (s0)
      yield s

    override def zeroReward: GridReward = 0
    val instances = GridInstances
/** Here is a proof that our types actually deliver on everything that an Agent
  * needs to be able to do to work in the framework.
  */
object GridInstances
  extends AgentConstraints[GridState, GridFiniteState, GridAction, GridReward, Randomized]:

  // import cats.{Eq, Monad}
  // import cats.kernel.BoundedEnumerable
  //
  // import org.scalacheck.Gen
  // import org.scalacheck.Arbitrary
  // import org.scalacheck.Arbitrary.arbitrary
  import examples.concrete.windygrid.GridAction._

  given enumAction: BoundedEnumerable[GridAction] =
    BoundedEnumerableFromList (U, L, R, D)

  given enumState: BoundedEnumerable[GridFiniteState] = {
    val ss = for {
        x <- Seq (1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
        y <- Seq (1, 2, 3, 4, 5, 6, 7)
    } yield GridState (x,y)
    BoundedEnumerableFromList (ss: _*)
  }

  given schedulerIsMonad: Monad[Randomized] =
    concrete.Randomized.randomizedIsMonad

  given canTestInScheduler: CanTestIn[Randomized] =
    concrete.Randomized.canTestInRandomized

  lazy val genGridState: Gen[GridState] = for {
    x <- Gen.choose(1,10)
    y <- Gen.choose(1,7)
  } yield GridState (x,y)

  given arbitraryState: Arbitrary[GridState] =
    Arbitrary (genGridState)

  given eqGridState: Eq[GridState] =
    Eq.fromUniversalEquals

  given arbitraryReward: Arbitrary[GridReward] = Arbitrary (Gen.double)

  //given arbitraryAction =
  //  Arbitrary (Gen.oneOf(U, D, L, R))

  given rewardArith: Arith[GridReward] = Arith.given_Arith_Double
