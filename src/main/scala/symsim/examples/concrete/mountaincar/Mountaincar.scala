package symsim
package examples.concrete.mountaincar

import symsim.concrete.Randomized


object MountainCar
  extends Agent[CarState, CarFiniteState, CarAction, CarReward, Randomized] {
    def roundAt(p: Int)(n: Double): Double = { val s = math pow (10, p); (math round n * s) / s }


    def isFinal (s: CarState): Boolean =
        s.p >= 0.5

    def discretize (s: CarState): CarFiniteState =  {
      require (s.p >= -1.2, s"s.p = ${s.p} is not within the boundaries")
      require (s.p <= 0.5, s"s.p = ${s.p} is not within the boundaries")
      require (s.v >= -1.5, s"s.v = ${s.v} is not within the boundaries")
      require (s.v <= 1.5, s"s.v = ${s.v} is not within the boundaries")

      val dp = roundAt(2)(-1.2+((((s.p + 1.2)/0.17).floor)*0.17))
      val dv = roundAt(2)(-1.5+(((s.v + 1.5)/0.3).floor)*0.3)

      CarState (Math.max(Math.min(dv.toDouble,1.5),-1.5), Math.max(Math.min(dp.toDouble,0.5),-1.2))
    }

    private def carReward (s: CarState) (a: CarAction): CarReward =
      if (s.p >= 0.5) 1
      else -0.1

    /** Granularity of the step in seconds */
    private val t: Double = 0.1
    /** Mountain Car mass */
    private val mass: Double = 0.2
    private val friction: Double = 0.3
    private val gravity:  Double = 9.8


    // TODO: this is now deterministic but eventually needs to be randomized
    def step (s: CarState) (a: CarAction): (CarState, CarReward) = {
      val v1 = Math.min(1.5, Math.max (s.v + (gravity * mass * Math.cos(3.0 * s.p) + (a/mass) - (friction*s.v)) * t, -1.5))
      val p1 = Math.max(-1.2,Math.min (s.p + (v1 * t), 0.5))
      val s1 = CarState (v=v1, p=p1)


      s1 -> carReward (s1) (a)

    }

    def initialize: Randomized[CarState] = for {
      p <- Randomized.between (-1.2, 0.5)
      v <- Randomized.between (-1.5, 1.5)
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
    BoundedEnumerableFromList (-0.2, 0.0, 0.2)

  implicit lazy val enumState: BoundedEnumerable[CarFiniteState] = {
    val ss = for {
        p0 <- Seq (-1.2,-1.03,-0.86,-0.69,-0.52,-0.35,-0.18,-0.01,0.16,0.33,0.5)
        v0 <- Seq (-1.5,-1.2,-0.9,-0.6,-0.3,0.0,0.3,0.6,0.9,1.2,1.5)
    } yield CarState (v=v0,p=p0)
    BoundedEnumerableFromList (ss: _*)
  }

  implicit lazy val schedulerIsMonad: Monad[Randomized] =
    concrete.Randomized.randomizedIsMonad

  implicit lazy val canTestInScheduler: CanTestIn[Randomized] =
    concrete.Randomized.canTestInRandomized

  lazy val genCarState: Gen[CarState] = for {
    p <- Gen.choose(-1.2,0.5)
    v <- Gen.choose(-1.5,1.5)
  } yield CarState (v, p)

  implicit lazy val arbitraryState: Arbitrary[CarState] =
    Arbitrary (genCarState)

  implicit lazy val eqCarState: Eq[CarState] =
    Eq.fromUniversalEquals

  implicit lazy val arbitraryAction =
    Arbitrary (Gen.double)

  implicit lazy val rewardArith: Arith[CarReward] =
    Arith.arithDouble
}
