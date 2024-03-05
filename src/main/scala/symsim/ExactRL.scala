package symsim

import cats.Monad
import cats.syntax.functor.*
import cats.syntax.flatMap.*
import cats.syntax.foldable.*
import cats.syntax.option.*
import symsim.concrete.Probability
import org.typelevel.paiges.Doc
import symsim.Arith.*
import spire.syntax.action

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
  def epsilon0: Probability
  def α: Double = this.alpha
  def ε0: Probability = this.epsilon0

  /** A decay function for ε (exploration probability).  Override to define. 
    * If you do not want decay use identity (mix in NoDecay)
    */
  def decay (ε: Probability): Probability


  /* Policy Learning */

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
  def learningEpoch (f: VF, s_t: State, a_t: Action): Scheduler[(vf.VF, State, Action)]

  /** Execute a full learning episode (until the final state of agent is
    * reached).
    */
  def learningEpisode(fR: (VF, List[VF], Probability), s_t: State)
    : Scheduler[(VF, List[VF], Probability)] =

    def done (f: VF, s: State, a: Action): Boolean = 
      agent.isFinal(s)

    val (f, qL_t, ε) = fR

    for
      a    <- chooseAction (ε) (f) (agent.observe (s_t))
      fin  <- Monad[Scheduler].iterateUntilM (f, s_t, a) (learningEpoch) (done)
      qL_tt = fin._1 :: qL_t
    yield (fin._1, qL_tt, decay (ε))


  /** Executes as many full learning episodes (until the final state of agent
   *  is reached) as the given state LazyList generates. The list should be
   *  finite. Learn is lazy, it just constructs a sampler that is able to sample 
   *  outcomes of learning.  Each sample costs executing an episode. We force 
   *  the evaluation when we are done to return the value (in runQ).
   *
   *  @param f   the initial value function (for instance a Q-table)
   *  @param q_l the initial (empty) list of past value functions (a log)
   *  @param ss  a lazy list of initial states for the episode. Should be finite.
   *
   *  @return    a scheduler (a sampler) of final Q-Tables and intermediate
   *             Q-tables that led to them
   */
  final def learn (f: VF, q_l: List[VF], ss: LazyList[State])
    : Scheduler[(VF, List[VF])] =
    val result = 
      ss.foldM[Scheduler, (VF, List[VF], Probability)] (f, q_l, ε0) (learningEpisode)
    result.map { (vf, history, ε) => (vf, history) }



  /* Policy Evaluation */

  /** Evaluate a single epoch from state s_t, the action is selected according 
   *  to the policy. 
   *
   *  @param p   The policy used to select an action
   *  @param s_t The state we are moving from (source state)
   *  @param r_t Reward accumulated so far
   *
   *  @return the target state rached and an updated reward
   */
  def evalEpoch (p: Policy) (s_t: State, r_t: Reward): Scheduler[(State, Reward)] =
    val arbitraryAction = agent.instances.allActions.head
    val a_t = p.getOrElse (agent.observe (s_t), arbitraryAction)
    agent.step (s_t) (a_t)
      .map { (s_tt, r_tt) => (s_tt, r_t + r_tt)}


  /** Evaluate a single episode until a final state, following the policy p, and
   *  starting in the state s_t.
   *
   *  @param p   The policy used to select actions
   *  @param s_0 The starting state
   *  @return the accumulated reward along the episode
   */
  def evalEpisode (p: Policy) (s_0: State): Scheduler[Reward] =
    def done (s: State, r: Reward): Boolean = agent.isFinal (s)
    Monad[Scheduler].iterateUntilM (s_0, Arith.arith[Reward].zero) (evalEpoch (p)) (done)
      .map { _._2 }

  /** Evaluate a policy p on the Schedule ss.  If you want to evaluate one state 
   *  you can just create a singleton schedule. If you want to evaluate the same 
   *  state several times (to assess variance/convregence in a randomized agent)
   *  then just repeat the same state in the schedule several times. If you want to 
   *  evaluate on many random states, just use a randomized scheduler producing 
   *  random states. 
   *
   *  @param p   The policy to evaluate
   *  @param ss  The schedule of initial states for subsequent episodes 
   *
   *  @return the schedule of obtained accumulated rewards. The returned
   *          schedule has the same structure/size as ss. The outer scheduler 
   *          is over the initial states (so stems from `ss`). The inner scheduler 
   *          is the schedule produced by the learning process.
   */
  final def evaluate (p: Policy, ss: Scheduler[State]): Scheduler[Scheduler[Reward]] =
    ss.map { s => evalEpisode (p) (s) }
