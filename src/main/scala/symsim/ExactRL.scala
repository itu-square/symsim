package symsim

import cats.Monad
import cats.syntax.functor.*
import cats.syntax.flatMap.*
import cats.syntax.foldable.*
import cats.syntax.option.*

import org.typelevel.paiges.Doc

/** An abstract representation of an ExactRL algorithm. By exact we mean
  * using an exact representation of the value function
  * approximation. It appears that in the future this type name will change, 
  * as the approximate representations will be handled here as well.
  *
  * To implement a RL algorithm you should extend this trait, mix in a value
  * function representation (a class implementing the ValueFunction trait), and
  * implement (override) the missing elements, chiefly `learningEpoch`.
  */
trait ExactRL[State, ObservableState, Action, Reward, Scheduler[_]]
  extends RL[ObservableState, Action],
    ValueFunction[ObservableState, Action, Reward, Scheduler]:

  val agent: Agent[State, ObservableState, Action, Reward, Scheduler]

  import agent.instances.given

  def alpha: Double

  /** A single step of the learning algorithm
    *
    * @param q the last Q-matrix
    * @param s_t current state
    * @return the updated matrix Q, the successor state, and a
    * reward difference (the size of the update performed)
    *
    * To acommodate Q-Learning and SARSA in a single design, we receive
    * the action a_t to the iteration, and we also produce the action a_t
    * for the next iteration.  This allows SARSA to pass over the 'lookahead'
    * to the next iteration and stay properly on policy.  In Q-Learning this
    * introduces a small presentation complication, not more.
    */
  def learningEpoch (f: VF, s_t: State, a_t: Action): Scheduler[(VF, State, Action)]

  /** Execute a full learning episode (until the final state of agent is
    * reached).
    */
  def learningEpisode (f: VF, s_t: State): Scheduler[VF] =
    def p (f: VF, s: State, a: Action): Boolean = agent.isFinal (s)
    for
      a <- chooseAction (f) (agent.observe (s_t))
      fin <- Monad[Scheduler].iterateUntilM (f, s_t, a) (learningEpoch) (p)
    yield fin._1

  /** Executes as many full learning episodes (until the final state of agent is
    * reached) as the given state scheduler generates.  For this method to work
    * the scheduler needs to be foldable, and we use foldRight with Eval, to
    * make the evaluation lazy. We force the evaluation when we
    * are done to return the value.  However, to my best understanding, if the
    * Scheduler is lazy then the evaluation is not really doing more than just
    * formulating the thunk of that scheduler.
    */
  final def learn (f: VF, ss: => Scheduler[State]): Scheduler[VF] =
    ss.foldM[Scheduler, VF] (f) (learningEpisode)

   
