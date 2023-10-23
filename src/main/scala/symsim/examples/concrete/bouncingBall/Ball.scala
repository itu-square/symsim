package symsim
package examples.concrete.bouncingBall

import symsim.concrete.Randomized

/**
 * ADD comments later
 */

case class BallState (v: Double, h: Double):
  override def toString: String = s"[v=$v, h=$h]"

type BallObservableState = BallState
type BallAction = Double
type BallReward = Double

object Ball
  extends Agent[BallState, BallObservableState, BallAction, BallReward, Randomized]
  with Episodic:

    /** The episode should be guaranteed to terminate after
      * TimeHorizon steps. This is used *only* *for* testing. It does
      * not actually termintae the episodes. It is a bug if they run
      * longer.
      */
    val TimeHorizon: Int = 2000

    /** Granularity of the step in seconds */
    private val t: Double = 0.2

    /** Minimum height that piston can affect*/
    private val H = 6.0

    /** Gravity constant */
    private val g = 9.8

    /** Damping factor */
    private val e = 0.85

    private val hitAffect = 5.0

    /** Evidence of type class membership for this agent. */
    val instances = BallInstances

    def isFinal (s: BallState): Boolean =
      s.h == 0.0 && s.v > 1 && s.v >= 0


    def observe (s: BallState): BallObservableState =
      require (s.h >= 0, s"s.p = ${s.h} is not non-negative")

      val dp = (s.h/2.0).floor * 2.0
      val dv = (s.v/2.0).floor * 2.0

      BallState (dv min 10.0, dp min 15.0)


    private def ballReward (s: BallState) (a: BallAction): BallReward =
      if s.h < H then -a
      else if e * s.h > H then -a else a-1


    // TODO: this is now deterministic but eventually needs to be randomized
    def step (s: BallState) (a: BallAction): Randomized[(BallState, BallReward)] =
      if s.h > H && a == 1 then {
        val h1 = s.h
        val v1 = if s.v > 0 then s.v - hitAffect else -hitAffect

        val s1 = BallState(v1, h1)
        Randomized.const(s1, ballReward(s1)(a))
      }else{
        val h1 = Math.max(s.h + s.v * t - 0.5 * g * t * t, 0.0)
        val v1 = if h1 == 0.0 then Math.sqrt(s.v * s.v + 2 * g * s.h) * e else s.v - g * t
        val v2 = if v1 < 0 && s.v > 0 then 0.0 else v1

        val s1 = BallState(v2, h1)
        Randomized.const(s1, ballReward(s1)(a))
      }



    def initialize: Randomized[BallState] = Randomized.const (BallState (0.0, 10.0))

end Ball


/** Here is a proof that our types actually deliver on everything that an Agent
  * needs to be able to do to work in the framework.
  */
object BallInstances
  extends AgentConstraints[BallState, BallObservableState, BallAction, BallReward, Randomized]:

  import cats.{Eq, Monad, Foldable}
  import cats.kernel.BoundedEnumerable

  import org.scalacheck.Gen
  import org.scalacheck.Arbitrary
  import org.scalacheck.Arbitrary.arbitrary

  given enumAction: BoundedEnumerable[BallAction] =
    BoundedEnumerableFromList (0.0, 1.0)

  given enumState: BoundedEnumerable[BallObservableState] =
    val ss = for
      v <- Seq (0.0, 5.0, 10.0, 15.0)
      h <- Seq (0.0, 5.0, 10.0)
    yield BallState (v, h)
    BoundedEnumerableFromList (ss*)

  given schedulerIsMonad: Monad[Randomized] =
    concrete.Randomized.randomizedIsMonad

  given schedulerIsFoldable: Foldable[Randomized] =
    concrete.Randomized.randomizedIsFoldable

  given canTestInScheduler: CanTestIn[Randomized] =
    concrete.Randomized.canTestInRandomized

  lazy val genBallState: Gen[BallState] = for
     v <- Gen.choose (-15.0, 15.0)
     p <- Gen.choose (0.0, 10.0)
  yield BallState (v, p)

  given arbitraryState: Arbitrary[BallState] = Arbitrary (genBallState)

  given eqBallState: Eq[BallState] = Eq.fromUniversalEquals

  given arbitraryReward: Arbitrary[BallReward] = Arbitrary (Gen.double)

  given rewardArith: Arith[BallReward] = Arith.arithDouble

end BallInstances
