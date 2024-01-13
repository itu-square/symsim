package symsim
package examples.concrete.cartpole

import symsim.concrete.Randomized2
import cats.syntax.all.*


/** The state of the CartPole, an abstract type shared by the continuous state
 *  of the environment, and the observable state type.
 *
 *  @constructor Tags the tuple as a CartPoleState
 *  @param cp the cart's position (1D)
 *  @param cv the cart's linear velocity
 *  @param pa the pole's angle 
 *  @param pv the pole's angular velocity
 */
abstract class CartPoleAbstractState (
  val cp: Double,
  val cv: Double,
  val pa: Double,
  val pv: Double
):

  require (cp >= CpMin, s"cp too low: ¬ ($pv ≥ $CpMin)")
  require (cp <= CpMax, s"cp too high: ¬ ($pv ≤ $CpMax)")
  require (cv >= CvMin, s"cv too low: ¬ ($pv ≥ $CvMin)")
  require (cv <= CvMax, s"cv too high: ¬ ($pv ≤ $CvMax)")
  require (pa >= PaMin, s"pa too low: ¬ ($pv ≥ $PaMin)")
  require (pa <= PaMax, s"pa too high: ¬ ($pv ≤ $PaMax)")
  require (pv >= PvMin, s"pv too low: ¬ ($pv ≥ $PvMin)")
  require (pv <= PvMax, s"pv too high: ¬ ($pv ≤ $PvMax)")

  override def toString: String = 
    s"[cart position=$cp, cart velocity=$cv,"
      + s"pole angle=$pa, pole angular velocity=$pv]"

/** The environment state. All its invariants are in the superclass. */
class CartPoleState (cp: Double, cv: Double, pa: Double, pv: Double)
  extends CartPoleAbstractState (cp, cv, pa, pv)



/** Observable state separated from the continuous state, even though
 *  structurally identical, to allow specifying different invariants for the
 *  constructor.
 */
class CartPoleObservableState (cp: Double, cv: Double, pa: Double, pv: Double)
  extends CartPoleAbstractState (cp, cv, pa, pv):
  require (CpCutPoints.contains (cp), "Cart position is not discrete")
  require (CvCutPoints.contains (cv), "Cart linear velocity is not discrete")
  require (PaCutPoints.contains (pa), "Pole angle is not discrete")
  require (PvCutPoints.contains (pv), "Pole angular velocity is not discrete")

  
type CartPoleAction = Int
type CartPoleReward = Double

/** A discretization of a continuous state components to selected cutpoints */
def closest (value: Double) (cutPoints: List[Double]): Double =
  cutPoints
    .find { value <= _ }
    .getOrElse (cutPoints.last)

val CpMin = -4.8
val CpMax =  4.8
val CvMin = -Double.MaxValue
val CvMax =  Double.MaxValue
val PaMin = -0.418
val PaMax =  0.418
val PvMin = -Double.MaxValue
val PvMax =  Double.MaxValue

val CpCutPoints =
  List (CpMin, -2.4, -1.0, -0.5,-0.2, -0.1, 0, 0.1, 0.2, 0.5, 1.0, 2.4, CpMax)
val CvCutPoints = 
  List (CvMin, -2, -1.5, -1.0, -0.7, -0.5, 0, 0.5, 0.7, 1.0, 1.5, 2, CvMax)
val PaCutPoints = 
  List (PaMin, -0.2, -0.15, -0.1, -0.05, 0, 0.05, 0.1, 0.15, 0.2, PaMax)
val PvCutPoints = 
  List (PvMin, -2, -1.5, -1.0, -0.7, -0.5, 0, 0.5, 0.7, 1.0, 1.5, 2, PvMax)

/** A balancing cartpole example agent (with discretization)
 *
 *  Based on: Razvan V. Florian.
 *  Correct equations for the dynamics of the cart-pole system.
 *  https://coneural.org/florian/papers/05_cart_pole.pdf
 */
class CartPole (using probula.RNG)
  extends Agent[CartPoleState, CartPoleObservableState, CartPoleAction, 
    CartPoleReward, Randomized2],
    Episodic:

    /** The episode guarantees to terminate after TimeHorizon steps. 
     *
     *  This is used *only* *for* testing. It does not actually terminate the
     *  episodes. It is a bug in the step function if they run longer.
     */
    val TimeHorizon: Int = 2000

    val Gravity        = 9.81
    val CartMass       = 1.0
    val PoleMass       = 0.1
    val TotalMass      = PoleMass + CartMass
    val PoleLength     = 0.5
    val ForceMagnitude = 10.0
    val τ              = 0.02                   // time granularity in seconds
    val θThreshold     = 12 * 2 * Math.PI / 360 // in radians
    val xThreshold     = 2.4

    def isFinal (s: CartPoleState): Boolean =
      s.cp < - xThreshold || s.cp > xThreshold || s.pa < - θThreshold 
        || s.pa > θThreshold

    def observe (s: CartPoleState): CartPoleObservableState =
      val dcp = closest (s.cp) (CpCutPoints)
      val dcv = closest (s.cv) (CvCutPoints)
      val dpa = closest (s.pa) (PaCutPoints)
      val dpv = closest (s.pv) (PvCutPoints)
      CartPoleObservableState (dcp, dcv, dpa, dpv)

    private def cartPoleReward (s: CartPoleState) (a: CartPoleAction)
      : CartPoleReward = 1.0

    def step (s: CartPoleState) (a: CartPoleAction)
      : Randomized2[(CartPoleState, CartPoleReward)] =
      require (instances.enumAction.membersAscending.contains (a))
      val cos_τ = Math.cos (s.pa)
      val sin_τ = Math.sin (s.pa)

      val temp = 
        (a * ForceMagnitude + PoleMass * PoleLength * s.pv * s.pv * sin_τ) 
        / TotalMass
      val τAcc = (Gravity * sin_τ - cos_τ * temp) 
        / (PoleLength * (4.0/3.0 - PoleMass * cos_τ * cos_τ / TotalMass))
      val xAcc = 
        temp - PoleMass * PoleLength * PoleMass * PoleLength * τAcc * cos_τ 
        / TotalMass

      val cp1 = s.cp + τ * s.cv
      val cv1 = s.cv + τ * xAcc
      val pa1 = s.pa + τ * s.pv
      val pv1 = s.pv + τ * τAcc
      val s1  = CartPoleState (cp1, cv1, pa1, pv1)
      Randomized2.const (s1, cartPoleReward (s1) (a))

    def initialize: Randomized2[CartPoleState] = (for
      cp <- Randomized2.between (-0.05, 0.05)
      cv <- Randomized2.between (-0.05, 0.05)
      pa <- Randomized2.between (-0.05, 0.05)
      pv <- Randomized2.between (-0.05, 0.05)
      s = CartPoleState (cp, cv, pa, pv) 
    yield s).filter { !this.isFinal (_) }

    /** Evidence of type class membership for this agent. */
    val instances = new CartPoleInstances

end CartPole


/** Here is a proof that our types actually deliver on everything that an Agent
  * needs to be able to do to work in the framework.
  */
class CartPoleInstances (using probula.RNG)
  extends AgentConstraints[CartPoleState, CartPoleObservableState, 
    CartPoleAction, CartPoleReward, Randomized2]:

  import cats.{Eq, Monad, Foldable}
  import cats.kernel.BoundedEnumerable
  import org.scalacheck.{Gen, Arbitrary}

  given enumAction: BoundedEnumerable[CartPoleAction] =
    BoundedEnumerableFromList (-1, 1)

  given enumState: BoundedEnumerable[CartPoleObservableState] =
    val ss: List[CartPoleObservableState] = for
      cp <- CpCutPoints
      cv <- CvCutPoints
      pa <- PaCutPoints
      pv <- PvCutPoints
    yield CartPoleObservableState (cp, cv, pa, pv)
    BoundedEnumerableFromList (ss*)

  given schedulerIsMonad: Monad[Randomized2] =
    concrete.Randomized2.randomizedIsMonad

  given canTestInScheduler: CanTestIn[Randomized2] =
    concrete.Randomized2.canTestInRandomized

  lazy val genCartPoleState: Gen[CartPoleState] = for
    cp <- Gen.choose[Double] (CpMin, CpMax)
    cv <- Gen.choose[Double] (CvMin, CvMax)
    pa <- Gen.choose[Double] (PaMin, PaMax)
    pv <- Gen.choose[Double] (PvMin, PvMax)
  yield CartPoleState (cp, cv, pa, pv)

  given arbitraryState: Arbitrary[CartPoleState] = Arbitrary (genCartPoleState)
  given eqCartPoleState: Eq[CartPoleState] = Eq.fromUniversalEquals
  given arbitraryReward: Arbitrary[CartPoleReward] = Arbitrary (Gen.double)
  given rewardArith: Arith[CartPoleReward] = Arith.arithDouble

end CartPoleInstances
