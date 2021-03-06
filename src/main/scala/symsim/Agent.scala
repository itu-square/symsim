package symsim

import cats.kernel.BoundedEnumerable
import cats.Monad

/** TODO: A possible refactoring: separate Agents from finite agents, where the
  *  latter would mix in discretization, and finiteness constraints on
  *  FiniteState, and Action, with the corresponding tests
  */
trait Agent[State, FiniteState, Action, Reward, Scheduler[_]] {

  /** Execute one training step.  If symbolic, it may result in multiple
    * successors and rewards.
    */

  def step (s: State) (a: Action): (State, Reward)

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

  def reward (s: State) (a: Action): Reward =
    (step (s) (a))._2

  /** Provide the initial state of the agent for the scheduling policy captured
    * by Scheduler[ ].  For instance, a `Randomized` scheduler can allow starting
    * the agent in a random state, which is used in many learning  scenarios.
    * You can use a `Constant` scheduler if the initial state of the agent is
    * fixed.
    */

  def initialize: Scheduler[State]

  /** We need to know that a scheduler is a monad to perform some testing on it
    */
  implicit def schedulerIsMonad: Monad[Scheduler]

  /** Enforce that actions are enumerable because otherwise we cannot represent
    * Q as a map (enumerabilityof states is already enforced by RL).
    *
    * This could possibly be factored out (see TODO on the top of the file).
    */

   implicit def enumAction: BoundedEnumerable[Action]

  /** Enforce that states are enumerable because otherwise we cannot
    * represent a policy as a map.
    */
  implicit def enumState: BoundedEnumerable[FiniteState]

  /** A representation of a zero reward (initial reward).  This
    * might be abstracting too match.  On the other hand, this can be used to
    * represent a symbolic zero reward, for instance.
    */
  def zeroReward: Reward
  // TODO: Spire seems to be the library for abstracting numerics in Scala

}
