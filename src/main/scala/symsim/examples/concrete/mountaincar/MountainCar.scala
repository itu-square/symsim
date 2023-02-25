package symsim
package examples.concrete.mountaincar

import Math.cos

import cats.{Eq, Foldable, Monad}
import cats.kernel.BoundedEnumerable

import org.scalacheck.{Arbitrary, Gen}

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
  override def toString: String = f"[v $v%+2.2f, p $p%+2.2f]"

type CarObservableState = CarState
type CarAction = Double
type CarReward = Double


object MountainCar
  extends Agent[CarState, CarObservableState, CarAction, CarReward, Randomized]
  with Episodic:

  val TimeHorizon: Int = 3000000

  def roundAt (p: Int) (n: Double): Double =
    val s = Math.pow (10, p)
    Math.round (n * s) / s


  def isFinal (s: CarState): Boolean =
    s.p >= 0.5


  def observe (s: CarState): CarObservableState =
    require (s.p >= -1.2, s"s.p = ${s.p} is not within the boundaries")
    require (s.p <= 0.5, s"s.p = ${s.p} is not within the boundaries")
    require (s.v >= -1.5, s"s.v = ${s.v} is not within the boundaries")
    require (s.v <= 1.5, s"s.v = ${s.v} is not within the boundaries")

    val dp = roundAt (2) (-1.2 + (((s.p + 1.2) / 0.17).floor) * 0.17)
    val dv = roundAt (2) (-1.5 + (((s.v + 1.5) / 0.30).floor) * 0.30)
    CarState (v = dv.min (1.5).max (-1.5), p = dp.min (0.5).max (-1.2))


  private def carReward (s: CarState) (a: CarAction): CarReward =
    if s.p >= 0.5 then 1.0 else -0.01


  /** Granularity of the step in seconds */
  private val dt: Double = 0.1
  private val mass: Double = 0.2
  private val friction: Double = 0.3
  private val gravity:  Double = 9.8


  // TODO: this is now deterministic but eventually needs to be randomized
  def step (s: CarState) (a: CarAction): Randomized[(CarState, CarReward)] =
    val v = s.v + (gravity * mass * cos (3.0 * s.p) + a / mass - friction * s.v) * dt
    val p = s.p + (v * dt)
    val (v1, p1) = if p < -1.2 then (-1.2, 0.0) else (v, p) 
    val s1 = CarState (v = v1.max (-1.5).min (1.5), 
                       p = p1.min (0.5))
    Randomized.const (s1 -> carReward (s1) (a))


  def initialize: Randomized[CarState] = for
    p <- Randomized.repeat (Randomized.between (-1.2, 0.5))
    v <- Randomized.repeat (Randomized.between (-1.5, 1.5))
    s  = CarState (v=v, p=p) 
         if !isFinal (s) 
  yield s

  val instances = MountainCarInstances

end MountainCar


/** Here is a proof that our types actually deliver on everything that an Agent
  * needs to be able to do to work in the framework.
  */
object MountainCarInstances
  extends AgentConstraints[CarState, CarObservableState, CarAction, CarReward, Randomized]:

  given enumAction: BoundedEnumerable[CarAction] =
    BoundedEnumerableFromList (-0.2, 0.0, 0.2)

  given enumState: BoundedEnumerable[CarObservableState] =
    val ss = for
      p0 <- Seq (-1.2, -1.03, -0.86, -0.69, -0.52, -0.35, -0.18, -0.01, 0.16, 0.33, 0.5)
      v0 <- Seq (-1.5, -1.2, -0.9, -0.6, -0.3, 0.0, 0.3, 0.6, 0.9, 1.2, 1.5)
    yield CarState (v = v0, p = p0)
    BoundedEnumerableFromList (ss*)


  given schedulerIsMonad: Monad[Randomized] = Randomized.randomizedIsMonad

  given schedulerIsFoldable: Foldable[Randomized] = Randomized.randomizedIsFoldable

  given canTestInScheduler: CanTestIn[Randomized] = Randomized.canTestInRandomized

  lazy val genCarState: Gen[CarState] = for
    p <- Gen.choose (-1.2, 0.5)
    v <- Gen.choose (-1.5, 1.5)
  yield CarState (v, p)

  given arbitraryState: Arbitrary[CarState] = Arbitrary (genCarState)

  given eqCarState: Eq[CarState] = Eq.fromUniversalEquals

  /** This is useful to limit as it is used in tests and
    * initialization of Q tables. If these values are unreasonably
    * large they will break statistical tests.
    */
  given arbitraryReward: Arbitrary[CarReward] = 
    Arbitrary (Gen.choose (-300.0, 300.0))

  given rewardArith: Arith[CarReward] = Arith.arithDouble

end MountainCarInstances
