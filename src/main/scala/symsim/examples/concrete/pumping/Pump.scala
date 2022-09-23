package symsim
package examples.concrete.pumping
/** A case study of control of a water extraction at Marbjerg (operated by HOFOR).
  *
  * Source: Andreas Holck Høeg-Petersen. Reinforcement Learning for Controlling
  * Groundwater Extraction. MSc thesis at the IT University of Copenhagen.
  * August 2021. (esp. Chapters 2 and 3)
  */

import symsim.concrete.Randomized


/** Constants as fit in the model of AHHP (reference above).
  *
  * They are called a, b, c, and d parameters in his report.
  */ 
private val c1: Double = 1.0 / 12
private val c2: Double = 0.15 / (c1 * 80 * 12 * 24 * 365)
private val c3: Double = 0.1 / (12 * 24 * 365)
private val c4: Double = 0.05

/** Size of the history window for the head-level */
private val k: Int = 5



/** Complete state of a pump, both observable and unobservable. */
case class PumpState (
  f: Double,          // Flow (speed of pumping, m^3/h), controlled
  h: Double,          // Head (water) level in the pump, sensor
  hm: Double,         // Head mean defined over k steps
  tl: Double,         // Tank level (the amount of water stored in the tank)
  t: Int,             // Time (epoch)
  w: Double,          // Water leve in the ground deposits (unobservable)
  phm: List[Double]): // Past head means (a sliding window of history) 

  override def toString: String =
    s"[flow=$f, head=$h, head mean=$hm, tank level=$tl, time=$t, "
      + s"water=$w, past head means=$phm]"

end PumpState



/** The state observable to the pump controller. */
case class ObservablePumpState (
  f: Double,   // Discretized flow action (current motor speed)
  h: Double,   // Discretized observable head level
  hm: Double,  // Discretized, derived head mean
  tl: Double): // Discretized observable tank level

  override def toString: String =
    s"[flow=$f, head=$h, head mean=$hm, tank level=$tl]"

end ObservablePumpState



type PumpAction = Double
type PumpReward = Double



object Pump
        extends Agent[PumpState, ObservablePumpState, PumpAction, PumpReward,
                Randomized] with Episodic:

    val TimeHorizon: Int = 1

    def isFinal (s: PumpState): Boolean =
        s.t == 24


    def discretize (s: PumpState): ObservablePumpState =
        require (s.tl >= 0, s"s.tl = ${s.tl} is non-negative")

        val df = closest (s.f) (List (120.0, 115.0, 110.0, 105.0, 100.0, 95.0,
            90.0, 85.0, 80.0, 75.0, 70.0, 65.0, 60.0, 55.0, 50.0, 0.0))
        val dh = closest (s.h) (List (10.84, 10.68, 10.52, 10.36, 10.2, 10.04,
            9.88, 9.72, 9.56, 9.4, 9.24, 9.08, 8.92, 8.76, 8.6, 8.44, 8.28, 8.12,
            7.96, 7.8, 7.64, 7.48, 7.32, 7.16, 7.0))
        val dhm = closest (s.hm) (List (10.84, 10.68, 10.52, 10.36, 10.2, 10.04,
            9.88, 9.72, 9.56, 9.4, 9.24, 9.08, 8.92, 8.76, 8.6, 8.44, 8.28, 8.12,
            7.96, 7.8, 7.64, 7.48, 7.32, 7.16, 7.0))
        val dtl = closest (s.tl) (List (2000.0, 1800.0, 1600.0, 1400.0, 1200.0,
            1000.0, 800.0, 600.0, 400.0, 200.0, 0.0))

        ObservablePumpState (df, dh, dhm, dtl)


    private def closest (item: Double) (list: List[Double]): Double =
        list.find (x => item >= x).getOrElse(list.last)


    private val HEAD_HARD_MIN: Double = 7
    private val TANK_CAPACITY: Double = 2000

    private def pumpReward (os: PumpState) (s: PumpState) (a: PumpAction): PumpReward =
        headReward (os) (s) (a) + tankReward (os) (s) (a) + flowReward (os) (s) (a)

    private def headReward (os: PumpState) (s: PumpState) (a: PumpAction): Double =
        if s.h < HEAD_HARD_MIN then -9999
        else - ((1 + (s.h - s.hm).abs) * (1 + (s.h - s.hm).abs))

    private def tankReward (os: PumpState) (s: PumpState) (a: PumpAction): Double =
        if s.t < 0 then -9999
        else if s.t > TANK_CAPACITY then -9999
        else 0

    private def flowReward (os: PumpState) (s: PumpState) (a: PumpAction): Double =
        if s.f != os.f then - 0.5
        else 0

    val amp: Double = 0.41852857594808646
    val freq: Double = 0.0000597030105413
    val phase: Double = -6347.109214682171


    override def step (s: PumpState) (a: PumpAction)
    : Randomized[(PumpState, PumpReward)] =
        require (instances.enumAction.membersAscending.contains (a))
        for
            nf <- Randomized.gaussian (0.0, 1.0)
            f1 = a + nf
            nd <- Randomized.gaussian (0.1, 0.01)
            cd <- getDemand(s.t + 1)
            d  = cd + nd
            tl1 = s.tl + c1 * (f1 - d)
            h1 = s.h + c4 * (s.w + (c1 * f1 / Math.PI))
            nw <- Randomized.gaussian (0.0, 1.0)
            w1 = s.w - c2 * (c1 * f1) + c3 + (amp * Math.sin (2 * Math.PI * (s.t + phase) / freq)) + nw
            t1 = s.t + 1
            hm1 = (1.0 / k) * s.phm.sum
            phm1 = (s.hm :: s.phm).slice (0, k)
            s1 = PumpState (f = f1, h = h1, hm = hm1, tl = tl1, t = t1, w = w1, phm = phm1)
            pr = pumpReward (s) (s1) (a)
        yield (s1, pr)


    def getDemand (t: Int): Randomized [Double] =
        if t < 5 then Randomized.between (5.0, 15.0)
        if t < 12 then Randomized.between (15.0, 45.0)
        if t < 22 then Randomized.between (20.0, 38.0)
        else Randomized.between (5.0, 20.0)


    def initialize: Randomized[PumpState] = for
        f <- Randomized.const(80)
        h <- Randomized.const(10)
        hm <- Randomized.const(10)
        tl <- Randomized.const(1000)
        t <- Randomized.const(0)
        w <- Randomized.const(9.11)
        phm <- Randomized.const(List(10.0, 10.0, 10.0, 10.0, 10.0))
        s0 = PumpState (f, h, hm, tl, t, w, phm)
        s <- if isFinal (s0) then initialize
        else Randomized.const (s0)
    yield s


    override def zeroReward: PumpReward = 0.0

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
        BoundedEnumerableFromList (0, 50, 55, 60, 65, 70, 75, 80, 85, 90)

    given enumState: BoundedEnumerable[ObservablePumpState] =
        val ss = for
            f <- Seq (0, 50, 55, 60, 65, 70, 75, 80, 85, 90, 95, 100, 105, 110,
                115, 120)
            h <- Seq (7.0, 7.16, 7.32, 7.48, 7.64, 7.8, 7.96, 8.12, 8.28, 8.44, 8.6,
                8.76, 8.92, 9.08, 9.24, 9.4, 9.56, 9.72, 9.88, 10.04, 10.2, 10.36,
                10.52, 10.68, 10.84)
            hm <- Seq (7.0, 7.16, 7.32, 7.48, 7.64, 7.8, 7.96, 8.12, 8.28, 8.44, 8.6,
                8.76, 8.92, 9.08, 9.24, 9.4, 9.56, 9.72, 9.88, 10.04, 10.2, 10.36,
                10.52, 10.68, 10.84)
            tl <- Seq (0, 200, 400, 600, 800, 1000, 1200, 1400, 1600, 1800, 2000)
        yield ObservablePumpState (f, h, hm, tl)
        BoundedEnumerableFromList (ss*)

    given schedulerIsMonad: Monad[Randomized] =
        concrete.Randomized.randomizedIsMonad

    given schedulerIsFoldable: Foldable[Randomized] =
        concrete.Randomized.randomizedIsFoldable

    given canTestInScheduler: CanTestIn[Randomized] =
        concrete.Randomized.canTestInRandomized

    lazy val genPumpState: Gen[PumpState] = for
        f <- Gen.choose (0, 120)
        h <- Gen.choose (7.0, 10.84)
        hm <- Gen.choose (7.0, 10.84)
        tl <- Gen.choose (0.0, 2000.0)
        t <- Gen.choose (0, 24)
        w <- Gen.choose (0.0, 9.11)
        phm <- Gen.listOfN (5, Gen.choose (7.0, 10.84))

    yield PumpState (f, h, hm, tl, t, w, phm)

    given arbitraryState: Arbitrary[PumpState] = Arbitrary (genPumpState)

    given eqPumpState: Eq[PumpState] = Eq.fromUniversalEquals

    given arbitraryReward: Arbitrary[PumpReward] = Arbitrary (Gen.double)

    given rewardArith: Arith[PumpReward] = Arith.arithDouble

end PumpInstances
