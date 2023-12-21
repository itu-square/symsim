package symsim

import cats.Monad
import cats.syntax.functor.*
import cats.syntax.flatMap.*
import cats.syntax.foldable.*
import cats.syntax.option.*

import symsim.concrete.Probability

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
  extends RL[ObservableState, Action, Reward, Scheduler]: 

  /** A value function implementation */
  override val vf: QTable[ObservableState, Action, Reward, Scheduler]

  /** An agent to learn from */
  val agent: Agent[State, ObservableState, Action, Reward, Scheduler]

  import agent.instances.given
  import vf.{VF, chooseAction}

  def alpha: Double
  def epsilon: Probability
  def α: Double = this.alpha
  def ε: Probability = this.epsilon

  /** A single step of the learning algorithm
    *
    * @param q the last Q-matrix
    * @param s_t current 
    * @return the updated matrix Q, the successor state, and a
    * reward difference (the size of the update performed)
    *
    * To acommodate Q-Learning and SARSA in a single design, we receive
    * the action a_t to the iteration, and we also produce the action a_t
    * for the next iteration.  This allows SARSA to pass over the 'lookahead'
    * to the next iteration and stay properly on policy.  In Q-Learning this
    * introduces a small presentation complication, not more.
    */
  def learningEpoch (f: VF, r_t: Reward, s_t: State, a_t: Action): Scheduler[(vf.VF, Reward, State, Action)]

  /** Execute a full learning episode (until the final state of agent is
    * reached).
    */
  def learningEpisode(fR: (VF, List[Reward], List[VF]), s_t: State): Scheduler[(VF, List[Reward], List[VF])] =
    def p(f: VF, r_acc: Reward, s: State, a: Action): Boolean = agent.isFinal(s)

    val f = fR._1
    val r_acc = fR._2
    val qL_t = fR._3
    for
      a <- chooseAction(ε)(f)(agent.observe(s_t))
      fin <- Monad[Scheduler].iterateUntilM(f, Arith.arith[Reward].zero, s_t, a)(learningEpoch)(p)
      rL = fin._2 :: r_acc
      qL_tt = fin._1 :: qL_t
    yield (fin._1, rL, qL_tt)

  /** Executes as many full learning episodes (until the final state of agent is
    * reached) as the given state scheduler generates.  For this method to work
    * the scheduler needs to be foldable, and we use foldRight with Eval, to
    * make the evaluation lazy. We force the evaluation when we
    * are done to return the value.  However, to my best understanding, if the
    * Scheduler is lazy then the evaluation is not really doing more than just
    * formulating the thunk of that scheduler.
    */
  final def learn (f: VF, r_l: List[Reward], q_l: List[VF], ss: => Scheduler[State]):
    Scheduler[(VF, List[Reward], List[VF])] =
      ss.foldM[Scheduler, (VF, List[Reward], List[VF])] (f, r_l, q_l) (learningEpisode)

   
