package symsim
package examples.concrete.pumping

import symsim.concrete.Randomized
import scala.math


/** Constants as fit in the model of AHHP (reference above).
  *
  * They are called a, b, c, and d parameters in his report.
  */ 
val c1: Double = 1.0 / 12
val c2: Double = 0.15 / (c1 * 80 * 12 * 24 * 365)
val c3: Double = 0.1 / (12 * 24 * 365)
val c4: Double = 0.05

/** Size of the history window for the head-level */
val MemoryWindowSize: Int = 5


/** Complete state of a pump, both observable and unobservable. */
case class PumpState (
  f:   Double,        // Flow (speed of pumping, m^3/h), controlled
  h:   Double,        // Head (water) level in the pump, sensor
  hm:  Double,        // Head mean defined over k steps
  tl:  Double,        // Tank level (the amount of water stored in the tank)
  t:   Int,           // Time (epoch)
  w:   Double,        // Water level in the ground deposits (unobservable)
  phm: List[Double]): // Past head means (a sliding window of history) 

  require (f >= FlowMin,  s"Flow is too low: $f")
  require (f <= FlowMax,  s"Flow is too high: $f")
  require (h <= HeadMax,  s"Head is too high: $h")
  require (hm >= HeadMin, s"Head mean is too low: $hm")
  require (hm <= HeadMax, s"Head mean is too high: $hm")
  require (w >= WaterMin, s"Water level is too low: $w")
  require (w <= WaterMax, s"Water level is too high: $w")

  override def toString: String =
    s"[flow=$f, head=$h, head mean=$hm, tank level=$tl, time=$t, "
      + s"water=$w, past head means=$phm]"

end PumpState


/** Discretize cut points for flow, head, and tank variables./
  *
  * We discretize to the first point on the list that is lower than the actual
  * value of the variable.  So we 'round down'. Values below the last element in
  * the cut-point list are rounded up (to the last value).
  *
  * For now, we have preconditions about low values in discretization so the
  * getOrElse (rounding up) should never happen.
  *
  * The discretization has been selected in the project cited in the top of
  * this file.
  */
def closest (value: Double) (cutPoints: List[Double]): Double =
  cutPoints
    .find { value >= _ }
    .getOrElse (cutPoints.last)

val HeadMin  = 7.0
val HeadMax  = 10.84
val FlowMin  = 0.0
val FlowMax  = 120.0
val TankMin  = 0.0
val TankMax  = 2000.0
val WaterMin = 0.0
val WaterMax = 9.11

val FlowCutPoints = List (FlowMax, 115.0, 110.0, 105.0, 100.0, 95.0,
  90.0, 85.0, 80.0, 75.0, 70.0, 65.0, 60.0, 55.0, 50.0, FlowMin)

val HeadCutPoints = List (HeadMax, 10.68, 10.52, 10.36, 10.2, 10.04,
  9.88, 9.72, 9.56, 9.4, 9.24, 9.08, 8.92, 8.76, 8.6, 8.44, 8.28, 
  8.12, 7.96, 7.8, 7.64, 7.48, 7.32, 7.16, HeadMin)

val TankCutPoints = List (TankMax, 1800.0, 1600.0, 1400.0, 
  1200.0, 1000.0, 800.0, 600.0, 400.0, 200.0, TankMin)

/** The (discrete) state, observable by the pump controller. */
case class ObservablePumpState (
  f: Double,   // Discretized flow action (current motor speed)
  h: Double,   // Discretized observable head level
  hm: Double,  // Discretized, derived head mean
  tl: Double): // Discretized observable tank level

  require (FlowCutPoints.contains (f), s"Wrong discrete flow: $f")
  require (HeadCutPoints.contains (h), s"Wrong discrete head level: $h")
  require (HeadCutPoints.contains (hm), s"Wrong discrete mean head level:  $hm")
  require (TankCutPoints.contains (tl), s"Wrong discrete tank level: $tl")

  override def toString: String =
    s"(flow=$f, head=$h, head mean=$hm, tank level=$tl)"

end ObservablePumpState


type PumpAction = Double
type PumpReward = Double


/** A case study of control of a water extraction at Marbjerg (operated by HOFOR).
  *
  * Source: Andreas Holck Høeg-Petersen. Reinforcement Learning for Controlling
  * Groundwater Extraction. MSc thesis at the IT University of Copenhagen.
  * August 2021. (esp. Chapters 2 and 3)
  */
object Pump extends 
  Agent[PumpState, ObservablePumpState, PumpAction, PumpReward, Randomized],
  Episodic:

  /** An upper bound on episode duration. Used only in testing */
  val TimeHorizon: Int = 20000


  /** If isFinal is true in a state, then the episode is complete */
  def isFinal (s: PumpState): Boolean =
    s.t >= 4000 || s.h < HeadMin || s.tl > TankMax || s.tl < TankMin


  def observe (s: PumpState): ObservablePumpState =
    require (s.tl >= TankMin, s"s.tl = ${s.tl} >= $TankMin")
    val df = closest (s.f) (FlowCutPoints)
    val dh = closest (s.h) (HeadCutPoints)
    val dhm = closest (s.hm) (HeadCutPoints)
    val dtl = closest (s.tl) (TankCutPoints)
    ObservablePumpState (df, dh, dhm, dtl)


  def reward (source: PumpState) (target: PumpState) (a: PumpAction): Double =
    if isFinal (target) then -9999.0
    else 
      val flowReward = if target.f != source.f then -0.5 else 0.0
      val headCost = (1 + (target.h - target.hm).abs) * (1 + (target.h - target.hm).abs)
      flowReward - headCost


  val Amp   = 0.41852857594808646
  val Freq  = 0.0000597030105413
  val Phase = -6347.109214682171


  override def step (s: PumpState) (a: PumpAction)
    : Randomized[(PumpState, PumpReward)] =
    require (instances.enumAction.membersAscending.contains (a))
    for
      nf  <- Randomized.gaussian (0.0, 1.0)
      f1   = a + nf
      nd  <- Randomized.gaussian (0.1, 0.01)
      cd  <- getDemand (s.t%24 + 1)
      d    = cd + nd
      tl1  = s.tl + c1 * (f1 - d)
      h1   = s.h + c4 * (s.w + (c1 * f1 / math.Pi))
      nw  <- Randomized.gaussian (0.0, 1.0)
      w1   = s.w - c2 * c1 * f1 + c3 + 
             (Amp * math.sin (2 * math.Pi * (s.t + Phase) / Freq)) + nw
      hm1  = (1.0/MemoryWindowSize) * s.phm.sum
      phm1 = (s.hm:: s.phm).slice (0, MemoryWindowSize)
      s1   = PumpState (f1, h1, hm1, tl1, s.t + 1, w1, phm1)
      pr   = reward (s) (s1) (a)
    yield (s1, pr)


  def demand (t: Int): Randomized[Double] =
    require (t >= 0 && t <= 24)
    if t < 5 then Randomized.between (5.0, 15.0)
    else if t < 12 then Randomized.between (15.0, 45.0)
    else if t < 22 then Randomized.between (20.0, 38.0)
    else Randomized.between (5.0, 20.0)


  def initialize: Randomized[PumpState] = for
    f   <- Randomized.const (80)
    h   <- Randomized.const (10.0)
    hm  <- Randomized.const (10)
    tl  <- Randomized.const (1000)
    w   <- Randomized.const (9.11)
    phm <- Randomized.const (List (10.0, 10.0, 10.0, 10.0, 10.0))
    s   =  PumpState (f, h, hm, tl, 0, w, phm)
      _ =  assert (!isFinal (s))
  yield s

  val instances = PumpInstances

end Pump


/** Here is a proof that our types actually deliver on everything that an Agent
  * needs to be able to do to work in the framework.
  */
object PumpInstances
  extends AgentConstraints[PumpState, ObservablePumpState, PumpAction, 
    PumpReward, Randomized]:

  import cats.{Eq, Monad, Foldable}
  import cats.kernel.BoundedEnumerable

  import org.scalacheck.Gen
  import org.scalacheck.Arbitrary
  import org.scalacheck.Arbitrary.arbitrary

  given enumAction: BoundedEnumerable[PumpAction] =
    BoundedEnumerableFromList (0.0, 50.0, 55.0, 60.0, 65.0, 70.0, 
      75.0, 80.0, 85.0, 90.0)

  given enumState: BoundedEnumerable[ObservablePumpState] =
    val ss: List[ObservablePumpState] = for
      f  <- FlowCutPoints
      h  <- HeadCutPoints 
      hm <- HeadCutPoints 
      tl <- TankCutPoints
    yield ObservablePumpState (f, h, hm, tl)
    BoundedEnumerableFromList (ss*)

  given schedulerIsMonad: Monad[Randomized] =
    concrete.Randomized.randomizedIsMonad

  given schedulerIsFoldable: Foldable[Randomized] =
    concrete.Randomized.randomizedIsFoldable

  given canTestInScheduler: CanTestIn[Randomized] =
    concrete.Randomized.canTestInRandomized

  lazy val genPumpState: Gen[PumpState] = for
    f   <- Gen.choose[Double] (FlowMin, FlowMax)
    h   <- Gen.choose[Double] (HeadMin, HeadMax)
    hm  <- Gen.choose[Double] (HeadMin, HeadMax)
    tl  <- Gen.choose[Double] (TankMin, TankMax)
    t   <- Gen.choose[Int] (0, 24)
    w   <- Gen.choose[Double] (WaterMin, WaterMax)
    phm <- Gen.listOfN (5, Gen.choose (HeadMin, HeadMax))
  yield PumpState (f, h, hm, tl, t, w, phm)

  given arbitraryState: Arbitrary[PumpState] = Arbitrary (genPumpState)

  given eqPumpState: Eq[PumpState] = Eq.fromUniversalEquals

  given arbitraryReward: Arbitrary[PumpReward] = Arbitrary (Gen.double)

  given rewardArith: Arith[PumpReward] = Arith.arithDouble

end PumpInstances
