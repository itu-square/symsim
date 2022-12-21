package symsim

import org.scalacheck.{Arbitrary, Gen}

import cats.{Foldable, Monad}
import cats.syntax.functor.*
import cats.kernel.BoundedEnumerable

/** TODO: A possible refactoring: separate Agents from finite agents, where the
  * latter would mix in discretization, and finiteness constraints on
  * ObservableState, and Action, with the corresponding tests
  */
trait Agent[State, ObservableState, Action, Reward, Scheduler[_]]:

  import instances.given

  /** The evidence required to consider a generic instance of Agent to be
    * an agent for the rest of the framework
    */
  val instances: AgentConstraints[State, ObservableState, Action, Reward, Scheduler]

  /** Execute one training step.  If symbolic, it may result in multiple
    * successors and rewards.
    */
  def step (s: State) (a: Action): Scheduler[(State, Reward)]


  /** True for final states (used for end-of-episode detection) */
  def isFinal (s: State): Boolean


  /** Maps the full state space to the type used in the representation of
    * policies.  In principle, this could have been moved from here to the type
    * representing concrete policies or concrete SARSA variants, but it is
    * convenient to maintain discretizations with the agent, as this makes the
    * examples compact.
    */
  def observe (s: State): ObservableState

  /** Return the reward assigned to state s when action a is given (the
    * reward estimation function)
    */
  def reward (s: State) (a: Action): Scheduler[Reward] =
    for outcome <- step (s) (a)
    yield outcome._2

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
  // TODO: This looks like something that could go together or fold into
  // instances (perhaps it is a property in Arith)






/** Type evidence required to consider a generic instance of Agent to be
  * an agent for the rest of the framework
  */
trait AgentConstraints[State, ObservableState, Action, Reward, Scheduler[_]]:

  /** Enforce that actions are enumerable because otherwise we cannot represent
    * Q as a map (enumerabilityof states is already enforced by RL).
    *
    * This could possibly be factored out (see TODO on the top of the file).
    */
  given enumAction: BoundedEnumerable[Action]

  /** Ensure that we use efficient lists (not iterating repeatedly) */
  lazy val allActions: List[Action] =
    enumAction.membersAscending.toList

  /** Enforce that states are enumerable because otherwise we cannot
    * represent a policy as a map.
    */
  given enumState: BoundedEnumerable[ObservableState]

  /** Ensure that we use efficient lists (not iterating repeatedly) */
  lazy val allObservableStates: List[ObservableState] =
    enumState.membersAscending.toList

  /** We need to be able to flatten branching created by some
   *  interpreters/schedulers.
    */
  given schedulerIsMonad: Monad[Scheduler]

  /** We need to be able to iterate over a schedule (fold) to sequence and merge
   *  schedule elements.
    */
  given schedulerIsFoldable: Foldable[Scheduler]

  /** We need to be able to run tests on scheduled values.
    */
  given canTestInScheduler: CanTestIn[Scheduler]

  /** We can generate random states for testing */
  given arbitraryState: Arbitrary[State]

  /** We can generate random finite states for testing */
  given asbitraryObservableState: Arbitrary[ObservableState] = 
    Arbitrary (Gen.oneOf (allObservableStates))

  /** We can generate random actions for testing */
  given arbitraryAction: Arbitrary[Action] = Arbitrary (Gen.oneOf (allActions))

  /** We can generate randome Reward values */
  given arbitraryReward: Arbitrary[Reward]

  /** Reward is an arithmetic type according to Arith */
  given rewardArith: Arith[Reward]
