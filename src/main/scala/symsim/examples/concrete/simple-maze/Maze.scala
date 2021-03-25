package symsim
package examples.concrete.simple-maze

import symsim.concrete.Randomized


object Maze
  extends Agent[MazeState, MazeFiniteState, MazeAction, MazeReward, Randomized] {

    def isFinal (s: MazeState): Boolean =
      s.h == 4.0 && (s.v == 2.0 || s.v == 3.0)

    // Maze is already discrete 
    def discretize (s: MazeState): MazeFiniteState =  { s }

    private def mazeReward (s: MazeState) (a: MazeAction): MazeReward =
      if s == MazeState(v=3,h=4) 10.0 //Good final state
      else if s == MazeState(v=2,h=4) -10.0 // Bad final state
      else -1.0

    // Does this even make sense for the maze? 
    /** Granularity of the step in seconds */
    private val t: Double = 1.0

    // TODO: this is now deterministic but eventually needs to be randomized
    def step (s: MazeState) (a: MazeAction): (MazeState, MazeReward) = { // Do cases over MazeAction
      val h1 = Math.min (s.h + s.v*t + 0.5*a*t*t, 10.0)
      val v1 = Math.max (s.v + a*t, 0.0)
      val s1 = MazeState (h1, v1)
      s1 -> mazeReward (s1) (a)
    }

// I would like to select the step-function based on an enumeration type of actions
    def stepUp(s: MazeState): MazeState =
      val v1 = if (s.h == 2 || s.v = 3) then s.v else s.v+1
      mazeState(v=v1,h=s.h)

    def stepDown(s: MazeState): MazeState =
      val v1 = if (s.h == 2 || s.v = 1) then s.v else s.v-1
      mazeState(v=v1,h=s.h)

    def stepLeft(s: MazeState): MazeState =
      val h1 = if (s.v == 2 || s.h = 1) then s.h else s.h-1
      mazeState(h=h1,v=s.v)

    def stepRight(s: MazeState): MazeState =
      val h1 = if (s = MazeState(v=1,h=2) || s.h = 4) then s.h else s.h-1
      mazeState(h=h1,v=s.v)

    def initialize: Randomized[MazeState] = for {
      v <- Randomized.between (1.0, 3.0)
      h <- Randomized.between (1.0, 4.0)
      s0 = MazeState (v,h)
      s <- if (isFinal (s0) || s == MazeState(2,2)) initialize
           else Randomized.const (s0)
    } yield s

    override def zeroReward: MazeReward = 0.0

    lazy val instances = MazeInstances
}


/** Here is a proof that our types actually deliver on everything that an Agent
  * needs to be able to do to work in the framework.
  */
object MazeInstances
  extends AgentConstraints[MazeState, MazeFiniteState, MazeAction, MazeReward, Randomized] {

  import cats.{Eq, Monad}
  import cats.kernel.BoundedEnumerable

  import org.scalacheck.Gen
  import org.scalacheck.Arbitrary
  import org.scalacheck.Arbitrary.arbitrary

  implicit lazy val enumAction: BoundedEnumerable[MazeAction] =
    BoundedEnumerableFromList (-10, -5, -2.5)

  implicit lazy val enumState: BoundedEnumerable[MazeFiniteState] = {
    val ss = for {
        v <- Seq (1.0, 2.0, 3.0)
        h <- Seq (1.0, 2.0, 3.0, 4.0)
    } yield MazeState (v,h)
    BoundedEnumerableFromList (ss: _*)
  }

  implicit lazy val schedulerIsMonad: Monad[Randomized] =
    concrete.Randomized.randomizedIsMonad

  implicit lazy val canTestInScheduler: CanTestIn[Randomized] =
    concrete.Randomized.canTestInRandomized

  lazy val genMazeState: Gen[MazeState] = for {
      v <- arbitrary[Double]
      h <- arbitrary[Double]
  } yield MazeState (Math.abs (v), Math.abs (h))

  implicit lazy val arbitraryState: Arbitrary[MazeState] =
    Arbitrary (genMazeState)

  implicit lazy val eqMazeState: Eq[MazeState] =
    Eq.fromUniversalEquals

  implicit lazy val arbitraryAction =
    Arbitrary (Gen.double)

  implicit lazy val rewardArith: Arith[MazeReward] =
    Arith.arithDouble
}
