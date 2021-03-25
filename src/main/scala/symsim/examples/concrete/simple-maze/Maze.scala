package symsim
package examples.concrete.simple-maze

import symsim.concrete.Randomized


object Maze
  extends Agent[MazeState, MazeFiniteState, MazeAction, MazeReward, Randomized] {

    def isFinal (s: MazeState): Boolean =
      s.v == 0.0 || Math.abs (s.p) >= 1000.0

    // Maze is discrete 
    def discretize (s: MazeState): MazeFiniteState =  { s }

    private def carReward (s: MazeState) (a: MazeAction): MazeReward =
      if (s.p >= 10.0) -10
      else if (s.p < 10.0 && s.v == 0.0) 10.0 - s.p
      else -1.0

    /** Granularity of the step in seconds */
    private val t: Double = 2.0

    // TODO: this is now deterministic but eventually needs to be randomized
    def step (s: MazeState) (a: MazeAction): (MazeState, MazeReward) = {
      val p1 = Math.min (s.p + s.v*t + 0.5*a*t*t, 10.0)
      val v1 = Math.max (s.v + a*t, 0.0)
      val s1 = MazeState (p1, v1)
      s1 -> carReward (s1) (a)
    }

    def initialize: Randomized[MazeState] = for {
      v <- Randomized.between (0.0, 10.0)
      p <- Randomized.between (0.0, 15.0)
      s0 = MazeState (v,p)
      s <- if (isFinal (s0)) initialize
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
        v <- Seq (0.0, 5.0, 10.0)
        p <- Seq (0.0, 5.0, 10.0, 15.0)
    } yield MazeState (v,p)
    BoundedEnumerableFromList (ss: _*)
  }

  implicit lazy val schedulerIsMonad: Monad[Randomized] =
    concrete.Randomized.randomizedIsMonad

  implicit lazy val canTestInScheduler: CanTestIn[Randomized] =
    concrete.Randomized.canTestInRandomized

  lazy val genMazeState: Gen[MazeState] = for {
      v <- arbitrary[Double]
      p <- arbitrary[Double]
  } yield MazeState (Math.abs (v), Math.abs (p))

  implicit lazy val arbitraryState: Arbitrary[MazeState] =
    Arbitrary (genMazeState)

  implicit lazy val eqMazeState: Eq[MazeState] =
    Eq.fromUniversalEquals

  implicit lazy val arbitraryAction =
    Arbitrary (Gen.double)

  implicit lazy val rewardArith: Arith[MazeReward] =
    Arith.arithDouble
}
