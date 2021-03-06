package symsim
package examples.concrete.mountaincar


import cats.{Eq, Monad}
import cats.kernel.BoundedEnumerable

import org.scalacheck.Gen
import org.scalacheck.Arbitrary

import symsim.concrete.Randomized
import Math.cos

/**
 * We map the car states to
 *
 *   { (0,5], (5,10], (10,15], (15,infty) } X { [0,5), [5,10), (10, infty) }.
 *
 * We represent these states by the left point of the interval, so we can use
 * the same type to represent the finite state space.
 */

case class CarState (v: Double, p: Double)
type CarFiniteState = CarState
type CarAction = Double
type CarReward = Double


object MountainCar
  extends Agent[CarState, CarFiniteState, CarAction, CarReward, Randomized]:

  def roundAt (p: Int) (n: Double): Double =
    val s = Math.pow (10, p)
    Math.round (n * s) / s


  def isFinal (s: CarState): Boolean = s.p >= 0.5


  def discretize (s: CarState): CarFiniteState =
    require (s.p >= -1.2, s"s.p = ${s.p} is not within the boundaries")
    require (s.p <= 0.5, s"s.p = ${s.p} is not within the boundaries")
    require (s.v >= -1.5, s"s.v = ${s.v} is not within the boundaries")
    require (s.v <= 1.5, s"s.v = ${s.v} is not within the boundaries")
    val dp = roundAt (2) (-1.2 + ((((s.p + 1.2)/0.17).floor)*0.17))
    val dv = roundAt (2) (-1.5 + (((s.v + 1.5)/0.3).floor)*0.3)
    CarState (v = dv.min (1.5).max (-1.5), p = dp.min (0.5).max (-1.2))


  private def carReward (s: CarState) (a: CarAction): CarReward =
    if s.p >= 0.5 then 1 else -0.1


  /** Granularity of the step in seconds */
  private val t: Double = 0.1
  /** Mountain Car mass */
  private val mass: Double = 0.2
  private val friction: Double = 0.3
  private val gravity:  Double = 9.8


  // TODO: this is now deterministic but eventually needs to be randomized
  def step (s: CarState) (a: CarAction): Randomized[(CarState, CarReward)] =
    val v = s.v + (gravity*mass*cos (3.0*s.p) + a/mass - friction*s.v) * t
    val v1 = v.max (-1.5).min (1.5)
    val p1 = Math.max (-1.2,Math.min (s.p + (v1 * t), 0.5))
    val s1 = CarState (v = v1, p = p1)
    Randomized.const (s1 -> carReward (s1) (a))


  def initialize: Randomized[CarState] = for
    p <- Randomized.between (-1.2, 0.5)
    v <- Randomized.between (-1.5, 1.5)
    s0 = CarState (v,p)
    s <- if isFinal (s0) then initialize else Randomized.const (s0)
  yield s

  override def zeroReward: CarReward = 0.0

  val instances = MountainCarInstances



/** Here is a proof that our types actually deliver on everything that an Agent
  * needs to be able to do to work in the framework.
  */
object MountainCarInstances
  extends AgentConstraints[CarState, CarFiniteState, CarAction, CarReward, Randomized]:

  given enumAction: BoundedEnumerable[CarAction] =
    BoundedEnumerableFromList (-0.2, 0.0, 0.2)

  given enumState: BoundedEnumerable[CarFiniteState] =
    val ss = for
      p0 <- Seq (-1.2, -1.03, -0.86, -0.69, -0.52, -0.35, -0.18, -0.01, 0.16, 0.33, 0.5)
      v0 <- Seq (-1.5, -1.2, -0.9, -0.6, -0.3, 0.0, 0.3, 0.6, 0.9, 1.2, 1.5)
    yield CarState (v=v0,p=p0)
    BoundedEnumerableFromList (ss: _*)


  given schedulerIsMonad: Monad[Randomized] =
    concrete.Randomized.randomizedIsMonad

  given canTestInScheduler: CanTestIn[Randomized] =
    concrete.Randomized.canTestInRandomized

  lazy val genCarState: Gen[CarState] = for
    p <- Gen.choose (-1.2,0.5)
    v <- Gen.choose (-1.5,1.5)
  yield CarState (v, p)

  given arbitraryState: Arbitrary[CarState] = Arbitrary (genCarState)

  given eqCarState: Eq[CarState] = Eq.fromUniversalEquals

  given arbitraryReward: Arbitrary[CarReward] = Arbitrary (Gen.double)

  given rewardArith: Arith[CarReward] = Arith.given_Arith_Double
