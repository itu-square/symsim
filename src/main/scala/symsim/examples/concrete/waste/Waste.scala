package symsim
package examples.concrete.waste

import symsim.concrete.Randomized

opaque type SectionID = Int

opaque type Meters = Double
opaque type CubicMeters = Double

trait Component:
  def initialize: Randomized[ComponentState]

trait Tank extends Component:
  def capacity: CubicMeters
  def initialize: Randomized[TankState]

case object Pump extends Component:
  def initialize: Randomized[PumpState]

case class Surface extends Tank
  val capacity: CubicMeters = Double.MaxValue

case class Section (
  next: Section,
  above: Surface,
  capacity: Double, 
  surfaceArea: Double,
  damper: Double,
  qIn: Double,
  id: SectionID) extends Tank:

  override Qout_ = c * sqrt (w) * damper // TODO: w, damper are state elements

  def sectionDynamics = 
    if (w >= capacity && Qin > Qout) || (w <= 0 && Qout > Qin) 
    then 0
    else Qin() - Qout()

  def Qout: Double = 
    if next.w == next.capactity 
    then 0
    else  Qout_ // a principle for a vertical tank with gravity
    // todo or if it is a pump next then plus + next_pump.outFlow
    //
  def Quc:
  if w[id] == Capacity [id]) return 0.0
  else return c * sqrt [this.above.w]

trait ComponentState: 
  def initialize: Exception
  /** The output flow of the component */
  def Qout: CubicMeters 
  /** The input flow of the component */
  def Qin: CubicMeters

type TankState = Meters // amount of water in the component 
type PumpState = CubicMeters // amount of water per s being pumped out now

type Observable = Exception

type Action = Exception

// TODO ConcreteRL should specialize rewards to Double
type Reward = Double

object Waste
  extends Agent[State, Observable, Action, Reward, Randomized]:
  // TODO: NonEpisodic seems to be needed

    /** Granularity of the step in seconds */
    private val t: Double = 2.0

    /** Evidence of type class membership for this agent. */
    val instances = CarInstances

    override val zeroReward: Reward = 0.0
 
    /** TODO: this is either inherited for nonepisodic, or not at all there! */
    def isFinal (s: State): Boolean = false

    def observe (s: State): ObservableState = ???

      val dp = (s.p/5.0).floor * 5.0
      val dv = (s.v/5.0).floor * 5.0

      CarState (dv min 10.0, dp min 15.0)

    def PumpToBIOFOS: 
      if Qout(3) < M1 then return 0
      else if (Qout(3) < M2) return bypassToSea;
      else return bypassToSeaCost * (Qout(N) - M1)
      else return bypasstpSeaCost * (M2- M1) + bypassToStream * (Qout(N) - M2)


    private def carReward (s: CarState) (a: CarAction): CarReward = ???
    // cost' == sum(id: ID_T) Quc(id) * w[id] >= Capacity[id] + PumpToBIOFOS()


    def step (s: CarState) (a: CarAction): Randomized[(CarState, CarReward)] =
      // Globals - this is going to the step function?
      // w' == sectionDynamics
      // s' = rain[id] - Quc(id)
      // w = Qin - Qout()


    def initialize: Randomized[State] = ???

    val instances = Instances

end Car


/** Here is a proof that our types actually deliver on everything that an Agent
  * needs to be able to do to work in the framework.
  */
object Instances
  extends AgentConstraints[State, Observable, Action, Reward, Randomized]:

  import cats.{Eq, Monad, Foldable}
  import cats.kernel.BoundedEnumerable

  import org.scalacheck.Gen
  import org.scalacheck.Arbitrary
  import org.scalacheck.Arbitrary.arbitrary

  given enumAction: BoundedEnumerable[Action] =
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
     v <- Gen.choose (0.0, Double.MaxValue)
     p <- Gen.choose (0.0, Double.MaxValue)
  yield CarState (v, p)

  given arbitraryState: Arbitrary[CarState] = Arbitrary (genCarState)

  given eqCarState: Eq[CarState] = Eq.fromUniversalEquals

  given arbitraryReward: Arbitrary[CarReward] = Arbitrary (Gen.double)

  given rewardArith: Arith[CarReward] = Arith.arithDouble

end CarInstances
