package symsim


/** TODO: A possible refactoring: separate Agents from finite agents, where the
  * latter would mix in discretization, and finiteness constraints on
  * FiniteState, and Action, with the corresponding tests
  */
trait Agent[State, FiniteState, Action, Reward, Scheduler[_]]:


  /** Execute one training step.  If symbolic, it may result in multiple
    * successors and rewards.
    */
  def step (s: State) (a: Action): (State, Reward)


  /** True for final states (used for episode end detection) */
  def isFinal (s: State): Boolean


  /** Maps the full state space to the type used in the representation of
    * policies.  In principle, this could have been moved from here to the type
    * representing concrete policies or concrete SARSA variants, but it is
    * convenient to maintain discretizations with the agent, as this makes the
    * examples compact.
    */
  def discretize (s: State): FiniteState


  /** Return the reward assigned to state s when action a is given (the
    * reward estimation function)
    */
  def reward (s: State) (a: Action): Reward = (step (s) (a))._2


  /** Provide the initial state of the agent for the scheduling policy captured
    * by Scheduler[ ].  For instance, a `Randomized` scheduler can allow starting
    * the agent in a random state, which is used in many learning  scenarios.
    * You can use a `Constant` scheduler if the initial state of the agent is
    * fixed.
    */
  def initialize: Scheduler[State]


  /** A representation of a zero reward (initial reward).  This
    * might be abstracting too match.  On the other hand, this can be used to
    * represent a symbolic zero reward, for instance.
    */
  def zeroReward: Reward
  // TODO: Spire seems to be the library for abstracting numerics in Scala


  /** Type evidence required to consider a generic instance of Agent to be
    * an agent for the rest of the framework
    */
  val instances: AgentConstraints[State, FiniteState, Action, Reward, Scheduler]




/** Type evidence required to consider a generic instance of Agent to be
  * an agent for the rest of the framework
  */
trait AgentConstraints[State, FiniteState, Action, Reward, Scheduler[_]]:

  import org.scalacheck.Arbitrary
  import org.scalacheck.Gen
  import cats.kernel.BoundedEnumerable
  import cats.Monad

  /** Enforce that actions are enumerable because otherwise we cannot represent
    * Q as a map (enumerabilityof states is already enforced by RL).
    *
    * This could possibly be factored out (see TODO on the top of the file).
    */
  implicit def enumAction: BoundedEnumerable[Action]

  /** Ensure that we use efficient lists (not iterating repeatedly) */
  lazy val allActions: List[Action] =
    enumAction.membersAscending.toList

  /** Enforce that states are enumerable because otherwise we cannot
    * represent a policy as a map.
    */
  implicit def enumState: BoundedEnumerable[FiniteState]

  /** Ensure that we use efficient lists (not iterating repeatedly) */
  lazy val allFiniteStates: List[FiniteState] =
    enumState.membersAscending.toList

  /** We need to know that a scheduler is a monad */
  implicit def schedulerIsMonad: Monad[Scheduler]

  implicit def canTestInScheduler: CanTestIn[Scheduler]

  /** We can generate random states for testing */
  implicit def arbitraryState: Arbitrary[State]

  /** We can generate random finite states for testing */
  implicit lazy val arbitraryFiniteState: Arbitrary[FiniteState] =
    Arbitrary (Gen.oneOf (allFiniteStates))

  /** We can generate random actions for testing */
  implicit lazy val arbitraryAction: Arbitrary[Action] =
    Arbitrary (Gen.oneOf (allActions))

  /** We can generate randome Reward values */
  implicit def arbitraryReward: Arbitrary[Reward]

  /** Reward is an arithmetic type according to Arith */
  implicit def rewardArith: Arith[Reward]
