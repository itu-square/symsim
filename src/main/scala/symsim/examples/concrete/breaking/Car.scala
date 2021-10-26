package symsim
package examples.concrete.breaking

import symsim.concrete.Randomized

/**
 * We map the car states to
 *
 *   { (0,5], (5,10], (10,15], (15,infty) } X { [0,5), [5,10), (10, infty) }.
 *
 * We represent these states by the left point of the interval, so we can use
 * the same type to represent the finite state space.
 */

case class CarState (v: Double, p: Double):
  override def toString: String = s"[v=$v, p=$p]"

type CarFiniteState = CarState
type CarAction = Double
type CarReward = Double

object Car
  extends Agent[CarState, CarFiniteState, CarAction, CarReward, Randomized]
  with Episodic:

    val TimeHorizon: Int = 2000

    def isFinal (s: CarState): Boolean =
      s.v == 0.0 || Math.abs (s.p) >= 1000.0


    def discretize (s: CarState): CarFiniteState =
      require (s.v >= 0, s"s.v = ${s.v} is not non-negative")
      require (s.p >= 0, s"s.p = ${s.p} is not non-negative")

      val dp = (s.p/5.0).floor * 5.0
      val dv = (s.v/5.0).floor * 5.0

      CarState (dv min 10.0, dp min 15.0)


    private def carReward (s: CarState) (a: CarAction): CarReward =
      if s.p >= 10.0 then -10
      else if s.p < 10.0 && s.v == 0.0 then 10.0 - s.p
      else -1.0


    /** Granularity of the step in seconds */
    private val t: Double = 2.0

    // TODO: this is now deterministic but eventually needs to be randomized
    def step (s: CarState) (a: CarAction): Randomized[(CarState, CarReward)] =
      // Stop moving when velecity is zero, breaking is not moving backwards
      val t1 = Math.min (- s.v / a, t)
      val p1 = Math.min (s.p + s.v*t1 + 0.5*a*t1*t1, 10.0)
      val v1 = Math.max (s.v + a*t1, 0.0)
      val s1 = CarState (p1, v1)
      Randomized.const (s1 -> carReward (s1) (a))


    def initialize: Randomized[CarState] = for
      v <- Randomized.between (0.0, 10.0)
      p <- Randomized.between (0.0, 15.0)
      s0 = CarState (v,p)
      s <- if isFinal (s0) then initialize
           else Randomized.const (s0)
    yield s


    override def zeroReward: CarReward = 0.0

    val instances = CarInstances

end Car


/** Here is a proof that our types actually deliver on everything that an Agent
  * needs to be able to do to work in the framework.
  */
object CarInstances
  extends AgentConstraints[CarState, CarFiniteState, CarAction, CarReward, Randomized]:

  import cats.{Eq, Monad, Foldable}
  import cats.kernel.BoundedEnumerable

  import org.scalacheck.Gen
  import org.scalacheck.Arbitrary
  import org.scalacheck.Arbitrary.arbitrary

  given enumAction: BoundedEnumerable[CarAction] =
    BoundedEnumerableFromList (-10, -5, -2.5, -0.5, -0.05, -0.01, -0.001)

  given enumState: BoundedEnumerable[CarFiniteState] =
    val ss = for
      v <- Seq (0.0, 5.0, 10.0)
      p <- Seq (0.0, 5.0, 10.0, 15.0)
    yield CarState (v,p)
    BoundedEnumerableFromList (ss: _*)

  given schedulerIsMonad: Monad[Randomized] =
    concrete.Randomized.randomizedIsMonad

  given schedulerIsFoldable: Foldable[Randomized] =
    concrete.Randomized.randomizedIsFoldable

  given canTestInScheduler: CanTestIn[Randomized] =
    concrete.Randomized.canTestInRandomized

  lazy val genCarState: Gen[CarState] = for
     v <- Gen.choose (0.0, Double.MaxValue)
     p <- Gen.choose (0.0, Double.MaxValue)
  yield CarState (v, p)

  given arbitraryState: Arbitrary[CarState] = Arbitrary (genCarState)

  given eqCarState: Eq[CarState] = Eq.fromUniversalEquals

  given arbitraryReward: Arbitrary[CarReward] = Arbitrary (Gen.double)

  given rewardArith: Arith[CarReward] = Arith.arithDouble

end CarInstances
