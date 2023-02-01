package symsim
package examples.concrete.cartpole

import symsim.concrete.Randomized

/**
 * https://coneural.org/florian/papers/05_cart_pole.pdf
 */

case class CartPoleState (cp: Double, cv: Double, pa: Double, pv: Double):
  override def toString: String = s"[cart position=$cp, cart velocity=$cv," +
          s"pole angle=$pa, pole angular velocity=$pv]"

type CartPoleObservableState = CartPoleState
type CartPoleAction = Int
type CartPoleReward = Double

def closest (value: Double) (cutPoints: List[Double]): Double =
  cutPoints
    .find { value <= _ }
    .getOrElse (cutPoints.last)

val CP_MIN = -4.8
val CP_MAX = 4.8
val CV_MIN = -Double.MaxValue
val CV_MAX = Double.MaxValue
val PA_MIN = -0.418
val PA_MAX = 0.418
val PV_MIN = -Double.MaxValue
val PV_MAX = Double.MaxValue

val cPCutPoints = List (CP_MIN, -2.4, -1.0, -0.5,-0.2, -0.1, 0, 0.1, 0.2, 0.5, 1.0, 2.4, CP_MAX)
val cVCutPoints = List (CV_MIN, -2, -1.5, -1.0, -0.7, -0.5, 0, 0.5, 0.7, 1.0, 1.5, 2, CV_MAX)
val pACutPoints = List (PA_MIN, -0.2, -0.15, -0.1, -0.05, 0, 0.05, 0.1, 0.15, 0.2, PA_MAX)
val pVCutPoints = List (PV_MIN, -2, -1.5, -1.0, -0.7, -0.5, 0, 0.5, 0.7, 1.0, 1.5, 2, PV_MAX)

object CartPole
  extends Agent[CartPoleState, CartPoleObservableState, CartPoleAction, CartPoleReward, Randomized]
  with Episodic:

    /** The episode should be guaranteed to terminate after
      * TimeHorizon steps. This is used *only* *for* testing. It does
      * not actually termintae the episodes. It is a bug if they run
      * longer.
      */
    val TimeHorizon: Int = 2000

    val gravity = 9.8
    val masscart = 1.0
    val masspole = 0.1
    val total_mass = masspole + masscart
    val length = 0.5
    val polemass_length = masspole * length
    val force_mag = 10.0
    val tau = 0.02
    val theta_threshold_radians = 12 * 2 * Math.PI / 360
    val x_threshold = 2.4

    /** Evidence of type class membership for this agent. */
    val instances = CartPoleInstances

    override val zeroReward: CartPoleReward = 0.0
 

    def isFinal (s: CartPoleState): Boolean =
      s.cp < -x_threshold || s.cp > x_threshold ||
        s.pa < -theta_threshold_radians || s.pa > theta_threshold_radians


    def observe (s: CartPoleState): CartPoleObservableState =
      val dcp = closest (s.cp) (cPCutPoints)
      val dcv = closest (s.cv) (cVCutPoints)
      val dpa = closest (s.pa) (pACutPoints)
      val dpv = closest (s.pv) (pVCutPoints)

      CartPoleState (dcp, dcv, dpa, dpv)


    private def cartPoleReward (s: CartPoleState) (a: CartPoleAction): CartPoleReward =
      1


    // TODO: this is now deterministic but eventually needs to be randomized
    def step (s: CartPoleState) (a: CartPoleAction): Randomized[(CartPoleState, CartPoleReward)] =
      require (instances.enumAction.membersAscending.contains (a))
      val costheta = Math.cos(s.pa)
      val sintheta = Math.sin(s.pa)

      val temp = (
        a * force_mag + polemass_length * Math.pow(s.pv, 2) * sintheta
        ) / total_mass
      val thetaacc = (gravity * sintheta - costheta * temp) / (
        length * (4.0 / 3.0 - masspole * Math.pow(costheta, 2) / total_mass)
        )
      val xacc = temp - polemass_length * thetaacc * costheta / total_mass

      val cp1 = s.cp + tau * s.cv
      val cv1 = s.cv + tau * xacc
      val pa1 = s.pa + tau * s.pv
      val pv1 = s.pv + tau * thetaacc


      val s1 = CartPoleState (cp = cp1, cv = cv1, pa = pa1, pv = pv1)
      Randomized.const (s1, cartPoleReward (s1) (a))


    def initialize: Randomized[CartPoleState] =
      for
        cp <- Randomized.repeat (Randomized.between (-0.05, 0.05))
        cv <- Randomized.repeat (Randomized.between (-0.05, 0.05))
        pa <- Randomized.repeat (Randomized.between (-0.05, 0.05))
        pv <- Randomized.between(-0.05, 0.05)
        s = CartPoleState (cp, cv, pa, pv) if !isFinal (s)
      yield s

end CartPole


/** Here is a proof that our types actually deliver on everything that an Agent
  * needs to be able to do to work in the framework.
  */
object CartPoleInstances
  extends AgentConstraints[CartPoleState, CartPoleObservableState, CartPoleAction, CartPoleReward, Randomized]:

  import cats.{Eq, Monad, Foldable}
  import cats.kernel.BoundedEnumerable

  import org.scalacheck.Gen
  import org.scalacheck.Arbitrary
  import org.scalacheck.Arbitrary.arbitrary

  given enumAction: BoundedEnumerable[CartPoleAction] =
    BoundedEnumerableFromList (-1, 1)

  given enumState: BoundedEnumerable[CartPoleObservableState] =
    val ss: List[CartPoleObservableState] = for
      cp <- cPCutPoints
      cv <- cVCutPoints
      pa <- pACutPoints
      pv <- pVCutPoints
    yield CartPoleState (cp, cv, pa, pv)
    BoundedEnumerableFromList(ss *)

  given schedulerIsMonad: Monad[Randomized] =
    concrete.Randomized.randomizedIsMonad

  given schedulerIsFoldable: Foldable[Randomized] =
    concrete.Randomized.randomizedIsFoldable

  given canTestInScheduler: CanTestIn[Randomized] =
    concrete.Randomized.canTestInRandomized

  lazy val genCartPoleState: Gen[CartPoleState] = for
    cp <- Gen.choose[Double] (CP_MIN, CP_MAX)
    cv <- Gen.choose[Double] (CV_MIN, CV_MIN)
    pa <- Gen.choose[Double] (PA_MIN, PA_MAX)
    pv <- Gen.choose[Double] (PV_MIN, PV_MAX)
  yield CartPoleState (cp, cv, pa, pv)

  given arbitraryState: Arbitrary[CartPoleState] = Arbitrary (genCartPoleState)

  given eqCartPoleState: Eq[CartPoleState] = Eq.fromUniversalEquals

  given arbitraryReward: Arbitrary[CartPoleReward] = Arbitrary (Gen.double)

  given rewardArith: Arith[CartPoleReward] = Arith.arithDouble

end CartPoleInstances
