package symsim
package examples.concrete.breaking

import cats.Monad
import cats.kernel.BoundedEnumerable
import symsim.concrete.Randomized

object Car
  extends Agent[CarState, CarFiniteState, CarAction, CarReward, Randomized] {

    implicit val enumAction: BoundedEnumerable[CarAction] =
      BoundedEnumerableFromList (-10, -5, -2.5)

    implicit val enumState: BoundedEnumerable[CarFiniteState] = {

      val ss = for {
          v <- Seq (0.0, 5.0, 10.0)
          p <- Seq (0.0, 5.0, 10.0, 15.0)
      } yield CarState (v,p)

      BoundedEnumerableFromList (ss: _*)
    }

    implicit val schedulerIsMonad: Monad[Randomized] =
      implicitly[Monad[Randomized]] // pick up the cats State instance

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

    def step (s: CarState) (a: CarAction): (CarState,CarReward) = {
      val p1 = Math.min (s.p + s.v*t + 0.5*a*t*t, 10.0)
      val v1 = Math.max (s.v + a*t, 0.0)
      val s1 = CarState (p1, v1)
      s1 -> carReward (s1) (a)
    }

    def initialize: Randomized[CarState] =
      for {
        v <- Randomized.between (0.0, 10.0)
        p <- Randomized.between (0.0, 15.0)
      } yield CarState (v,p)

    override def zeroReward: CarReward = 0.0
}
