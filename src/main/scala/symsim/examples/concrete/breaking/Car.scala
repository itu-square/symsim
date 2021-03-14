package symsim
package examples.concrete.breaking

import symsim.concrete.Randomized


object Car
  extends Agent[CarState, CarFiniteState, CarAction, CarReward, Randomized] {

    def isFinal (s: CarState): Boolean =
      s.v == 0.0 || Math.abs (s.p) >= 1000.0

    def discretize (s: CarState): CarFiniteState =  {
      require (s.v >= 0, s"s.v = ${s.v} is not non-negative")
      require (s.p >= 0, s"s.p = ${s.p} is not non-negative")

      val dp = (s.p/5.0).floor * 5.0
      val dv = (s.v/5.0).floor * 5.0

      CarState (dv min 10.0, dp min 15.0)
    }

    private def carReward (s: CarState) (a: CarAction): CarReward =
      if (s.p >= 10.0) -10
      else if (s.p < 10.0 && s.v == 0.0) 10.0 - s.p
      else -1.0

    /** Granularity of the step in seconds */
    private val t: Double = 2.0

    // TODO: this is now deterministic but eventually needs to be randomized
    def step (s: CarState) (a: CarAction): (CarState, CarReward) = {
      val p1 = Math.min (s.p + s.v*t + 0.5*a*t*t, 10.0)
      val v1 = Math.max (s.v + a*t, 0.0)
      val s1 = CarState (p1, v1)
      s1 -> carReward (s1) (a)
    }

    def initialize: Randomized[CarState] = for {
      v <- Randomized.between (0.0, 10.0)
      p <- Randomized.between (0.0, 15.0)
      s0 = CarState (v,p)
      s <- if (isFinal (s0)) initialize
           else Randomized.const (s0)
    } yield s

    override def zeroReward: CarReward = 0.0

    lazy val instances = CarInstances
}


/** Here is a proof that our types actually deliver on everything that an Agent
  * needs to be able to do to work in the framework.
  */
object CarInstances
  extends AgentConstraints[CarState, CarFiniteState, CarAction, CarReward, Randomized] {

  import cats.{Eq, Monad}
  import cats.kernel.BoundedEnumerable

  import org.scalacheck.Gen
  import org.scalacheck.Arbitrary
  import org.scalacheck.Arbitrary.arbitrary

  implicit lazy val enumAction: BoundedEnumerable[CarAction] =
    BoundedEnumerableFromList (-10, -5, -2.5)

  implicit lazy val enumState: BoundedEnumerable[CarFiniteState] = {
    val ss = for {
        v <- Seq (0.0, 5.0, 10.0)
        p <- Seq (0.0, 5.0, 10.0, 15.0)
    } yield CarState (v,p)
    BoundedEnumerableFromList (ss: _*)
  }

  implicit lazy val schedulerIsMonad: Monad[Randomized] =
    concrete.Randomized.randomizedIsMonad

  implicit lazy val canTestInScheduler: CanTestIn[Randomized] =
    concrete.Randomized.canTestInRandomized

  lazy val genCarState: Gen[CarState] = for {
      v <- Arbitrary.arbDouble.arbitrary
      p <- Arbitrary.arbDouble.arbitrary
  } yield CarState (Math.abs (v), Math.abs (p))

  implicit lazy val arbitraryState: Arbitrary[CarState] =
    Arbitrary (genCarState)

  implicit lazy val arbitraryFiniteState: Arbitrary[CarFiniteState] =
    Arbitrary (Gen.oneOf (enumState.membersAscending))

  implicit lazy val eqCarState: Eq[CarState] =
    Eq.fromUniversalEquals

  implicit lazy val arbitraryAction =
    Arbitrary (Gen.double)

  implicit lazy val arbitraryReward =
    Arbitrary (Gen.double)

  implicit lazy val rewardArith: Arith[CarReward] =
    Arith.arithDouble
}
