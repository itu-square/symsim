package symsim
package examples.concrete.braking

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

type CarObservableState = CarState
type CarAction = Double
type CarReward = Double

object Car
  extends Agent[CarState, CarObservableState, CarAction, CarReward, Randomized]
  with Episodic:

    /** The episode should be guaranteed to terminate after
      * TimeHorizon steps. This is used *only* *for* testing. It does
      * not actually termintae the episodes. It is a bug if they run
      * longer.
      */
    val TimeHorizon: Int = 2000

    /** Granularity of the step in seconds */
    private val t: Double = 2.0

    /** Evidence of type class membership for this agent. */
    val instances = CarInstances

    def isFinal (s: CarState): Boolean =
      s.v == 0.0 || s.p >= 10 || Math.abs (s.p) >= 1000.0


    def observe (s: CarState): CarObservableState =
      require (s.v >= 0, s"s.v = ${s.v} is not non-negative")
      require (s.p >= 0, s"s.p = ${s.p} is not non-negative")

      val dp = (s.p/5.0).floor * 5.0
      val dv = (s.v/5.0).floor * 5.0

      CarState (dv min 10.0, dp min 15.0)


    private def carReward (s: CarState) (a: CarAction): CarReward =
      if s.p >= 10.0 then -100
      else a


    // TODO: this is now deterministic but eventually needs to be randomized
    def step (s: CarState) (a: CarAction): Randomized[(CarState, CarReward)] =
      require (instances.enumAction.membersAscending.contains (a))
      // Stop moving when velecity is zero, braking is not moving backwards
      val t1 = Math.min (- s.v / a, t)
      val p1 = Math.min (s.p + s.v*t1 + 0.5*a*t1*t1, 10.0)
      val v1 = if p1 >= 10 then 0
               else Math.max(s.v + a * t1, 0.0)
      val s1 = CarState (v = v1, p = p1)
      Randomized.const (s1, carReward (s1) (a))


    def initialize: Randomized[CarState] = for
      v <- Randomized.repeat (Randomized.between (0.0, 15.0))
      p <- Randomized.between (0.0, 20.0)
      s = CarState (v, p) if !isFinal (s)
    yield s

end Car


/** Here is a proof that our types actually deliver on everything that an Agent
  * needs to be able to do to work in the framework.
  */
object CarInstances
  extends AgentConstraints[CarState, CarObservableState, CarAction, CarReward, Randomized]:

  import cats.{Eq, Monad, Foldable}
  import cats.kernel.BoundedEnumerable

  import org.scalacheck.Gen
  import org.scalacheck.Arbitrary
  import org.scalacheck.Arbitrary.arbitrary

  given enumAction: BoundedEnumerable[CarAction] =
    BoundedEnumerableFromList (-10, -5, -2.5, -0.5, -0.05, -0.01, -0.001)

  given enumState: BoundedEnumerable[CarObservableState] =
    val ss = for
      v <- Seq (0.0, 5.0, 10.0)
      p <- Seq (0.0, 5.0, 10.0, 15.0)
    yield CarState (v,p)
    BoundedEnumerableFromList (ss*)

  given schedulerIsMonad: Monad[Randomized] =
    concrete.Randomized.randomizedIsMonad

  given schedulerIsFoldable: Foldable[Randomized] =
    concrete.Randomized.randomizedIsFoldable

  given canTestInScheduler: CanTestIn[Randomized] =
    concrete.Randomized.canTestInRandomized

  lazy val genCarState: Gen[CarState] = for
     v <- Gen.choose (0.0, 10.0)
     p <- Gen.choose (0.0, 10.0)
  yield CarState (v, p)

  given arbitraryState: Arbitrary[CarState] = Arbitrary (genCarState)

  given eqCarState: Eq[CarState] = Eq.fromUniversalEquals

  given arbitraryReward: Arbitrary[CarReward] = Arbitrary (Gen.double)

  given rewardArith: Arith[CarReward] = Arith.arithDouble

end CarInstances
