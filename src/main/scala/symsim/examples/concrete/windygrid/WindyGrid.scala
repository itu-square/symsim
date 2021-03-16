package symsim
package examples.concrete.windygrid

import symsim.concrete.Randomized
import examples.concrete.windygrid.GridAction._

object WindyGrid
  extends Agent[GridState, GridFiniteState, GridAction, GridReward, Randomized] {

    val windspec = Array.fill(10){scala.util.Random.nextInt(3)}

    def isFinal (s: GridState): Boolean =
      (s.x,s.y) == (8,4)

    def discretize (s: GridState): GridFiniteState =  {
      GridState (s.x, s.y)
    }


    private def GridReward (s: GridState) (a: GridAction): GridReward = -1.0


    private def stepRight (s: GridState): GridState ={
      val x1 = Math.max(Math.min(s.x+1,10),1)
      val y1 = Math.max(Math.min(s.y+windspec(s.x-1),7),1)
      GridState (x1, y1)
    }

    private def stepLeft (s: GridState): GridState ={
      val x1 = Math.min(Math.max(s.x-1,1),10)
      val y1 = Math.max(Math.min(s.y+windspec(s.x-1),7),1)
      GridState (x1, y1)
    }

    private def stepUp (s: GridState): GridState ={
      val x1 = s.x
      val y1 = Math.max(Math.min(s.y+1+windspec(s.x-1),7),1)
      GridState (x1, y1)
    }

    private def stepDown (s: GridState): GridState ={
      val x1 = s.x
      val y1 = Math.min(Math.max(s.y-1+windspec(s.x-1),1),7)
      GridState (x1, y1)
    }

    /** Granularity of the step in seconds */
    // TODO: this is now deterministic but eventually needs to be randomized
    def step (s: GridState) (a: GridAction): (GridState, GridReward) = {
    
      val s1={ if(a==R){
                      stepRight(s)
                   }
                  else if (a==L){
                      stepLeft(s)
                   }
                  else if (a==U){
                      stepUp(s)
                   }
                  else{
                      stepDown(s)
                  }
      }
      s1 -> GridReward (s1) (a)
    }

    def initialize: Randomized[GridState] = for {
      x <- Randomized.between(1,11)
      y <- Randomized.between(1,8)
      s0 = GridState (x,y)
      s <- if (isFinal (s0)) initialize
           else Randomized.const (s0)
    } yield s

    override def zeroReward: GridReward = 0

    lazy val instances = GridInstances
}


/** Here is a proof that our types actually deliver on everything that an Agent
  * needs to be able to do to work in the framework.
  */
object GridInstances
  extends AgentConstraints[GridState, GridFiniteState, GridAction, GridReward, Randomized] {

  import cats.{Eq, Monad}
  import cats.kernel.BoundedEnumerable

  import org.scalacheck.Gen
  import org.scalacheck.Arbitrary
  import org.scalacheck.Arbitrary.arbitrary

  implicit lazy val enumAction: BoundedEnumerable[GridAction] =
    BoundedEnumerableFromList (U, L, R, D)

  implicit lazy val enumState: BoundedEnumerable[GridFiniteState] = {
    val ss = for {
        x <- Seq (1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
        y <- Seq (1, 2, 3, 4, 5, 6, 7)
    } yield GridState (x,y)
    BoundedEnumerableFromList (ss: _*)
  }

  implicit lazy val schedulerIsMonad: Monad[Randomized] =
    concrete.Randomized.randomizedIsMonad

  implicit lazy val canTestInScheduler: CanTestIn[Randomized] =
    concrete.Randomized.canTestInRandomized

  lazy val genGridState: Gen[GridState] = for {
    x <- Gen.choose(1,10)
    y <- Gen.choose(1,7)
  } yield GridState (x,y)

  implicit lazy val arbitraryState: Arbitrary[GridState] =
    Arbitrary (genGridState)

  implicit lazy val eqCarState: Eq[GridState] =
    Eq.fromUniversalEquals

  implicit lazy val arbitraryAction =
    Arbitrary (Gen.oneOf(U, D, L, R))

  implicit lazy val rewardArith: Arith[GridReward] =
    Arith.arithDouble
}
