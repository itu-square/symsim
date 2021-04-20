package symsim

import org.scalacheck.Arbitrary
import org.scalacheck.Gen
import cats.kernel.BoundedEnumerable
import cats.Monad
import cats.syntax.monad._
import cats.syntax.functor._


/** TODO: A possible refactoring: separate Agents from finite agents, where the
  * latter would mix in discretization, and finiteness constraints on
  * FiniteState, and Action, with the corresponding tests
  */
trait Agent[State, FiniteState, Action, Reward, Scheduler[_]]:

  import instances.schedulerIsMonad

  /** Execute one training step.  If symbolic, it may result in multiple
    * successors and rewards.
    */
  def step (s: State) (a: Action): Scheduler[(State, Reward)]


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
  def reward (s: State) (a: Action): Scheduler[Reward] =
    for outcome <- step (s) (a) yield outcome._2

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
  val instances: AgentConstraints[State, FiniteState, Action, Reward, Scheduler]




/** Type evidence required to consider a generic instance of Agent to be
  * an agent for the rest of the framework
  */
trait AgentConstraints[State, FiniteState, Action, Reward, Scheduler[_]]:

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
  given enumState: BoundedEnumerable[FiniteState]

  /** Ensure that we use efficient lists (not iterating repeatedly) */
  lazy val allFiniteStates: List[FiniteState] =
    enumState.membersAscending.toList

  /** We need to know that a scheduler is a monad */
  given schedulerIsMonad: Monad[Scheduler]

  given canTestInScheduler: CanTestIn[Scheduler]

  /** We can generate random states for testing */
  given arbitraryState: Arbitrary[State]

  /** We can generate random finite states for testing */
  given Arbitrary[FiniteState] = Arbitrary (Gen.oneOf (allFiniteStates))

  /** We can generate random actions for testing */
  given Arbitrary[Action] = Arbitrary (Gen.oneOf (allActions))

  /** We can generate randome Reward values */
  given arbitraryReward: Arbitrary[Reward]

  /** Reward is an arithmetic type according to Arith */
  given rewardArith: Arith[Reward]
