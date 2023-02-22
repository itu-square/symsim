package symsim

import cats.Foldable
import cats.Monad
import cats.syntax.flatMap.*
import cats.syntax.foldable.*
import cats.syntax.functor.*

import symsim.concrete.Probability

// To allow arithmetic on reward values
import symsim.Arith.*

/** A BDL term for an estimation step. */
enum Est: 
  case Sample (gamma: Double)
  case Expectation (gamma: Double)

  def γ: Double = this match 
    case Sample (gamma) => gamma
    case Expectation (gamma) => gamma

import Est.*

/** A BDL term corresponding to an entire back-up diagram 
 *
 *  @param est a sequence of predictive estimation steps (these are executed)
 *  @param alpha the learning rate parameter of a RL update 
 *  @param update the final update --- the last step in the diagram
 */
case class Update (est: List[Est], alpha: Double, update: Est):
  def α: Double = this.alpha


/** A learning algorithm defined by a backup diagram (or in other
 *  words an interpreter for a BDL term.
 */
trait BdlLearn[State, ObservableState, Action, Reward, Scheduler[_]]
  extends ExactRL[State, ObservableState, Action, Reward, Scheduler]:

  import vf.*
 
  import agent.instances.given
  import agent.instances.*

  def epsilon: Probability
  def episodes: Int
  def bdl: Update 

  override def toString: String =
    s"BDL(..., 𝜀=$epsilon, $episodes episodes)"

  /** A single step of the learning algorithm
   *
   *  @param q   the last Q-matrix
   *  @param s_t current state
   *  @return the updated matrix Q, the successor state, and a
   *          reward difference (the size of the update performed)
   */
  def learningEpoch (q_t: VF, s_t: State, a_t: Action)
    : Scheduler[(VF, State, Action)] = bdl.update match 
  case Sample (γ) => 
    for 
      (s_tk, a_tk, g_tk, γ_tk) <- sem (bdl.est) (q_t) 
                                    (s_t, a_t, arith[Reward].zero, 1.0)
      (s_tkk, r_tkk)           <- agent.step (s_tk) (a_tk)
      (os_t, os_tkk)           =  (agent.observe (s_t), agent.observe (s_tkk))
      a_tkk                    <- vf.chooseAction (epsilon) (q_t) (os_tkk)
      g_tkk                    =  g_tk + γ_tk * r_tkk 
                                  + γ * γ_tk * q_t (os_tkk, a_tkk)
      q_t_value                =  q_t (os_t, a_t)
      q_tt_value               =  q_t_value + bdl.α * (g_tkk - q_t_value)
      q_tt                     =  q_t.updated (os_t, a_t, q_tt_value)
    yield (q_tt, s_tkk, a_tkk)

  case Expectation (γ) => 
    for 
      (s_tk, a_tk, g_tk, γ_tk) <- sem (bdl.est) (q_t) 
                                    (s_t, a_t, arith[Reward].zero, 1.0)
      (s_tkk, r_tkk)           <- agent.step (s_tk) (a_tk)
      (os_t, os_tkk)           =  (agent.observe (s_t), agent.observe (s_tkk))
      a_tkk                    <- vf.chooseAction (epsilon) (q_t) (os_tkk)
      expectation              = allActions
                                  .map { a => 
                                    vf.probability (epsilon) (q_t) (os_tkk, a)
                                      * q_t (os_tkk, a) }
                                  .arithSum
      g_tkk                    = g_tk + γ_tk * r_tkk + γ * γ_tk * expectation
      q_t_value                = q_t (os_t, a_t)
      q_tt_value               = q_t_value + bdl.α * (g_tkk - q_t_value)
      q_tt                     = q_t.updated (os_t, a_t, q_tt_value)
    yield (q_tt, s_tkk, a_tkk)
      

  /** Semantics of a sequence of estimation steps. */
  def sem (ests: List[Est]) (q_t: VF) 
    (s_t: State, a_t: Action, g_t: Reward, γ_t: Double)
    : Scheduler[(State, Action, Reward, Double)]= 

    ests.foldM[Scheduler, (State, Action, Reward, Double)]
      (s_t, a_t, g_t, γ_t)
      { case ((s_t, a_t, g_t, γ_t), e) => sem (e) (q_t) (s_t, a_t, g_t, γ_t) }


  /** Semantics of a single estimation step.
   *
   *  @param est the step term we are interpreting
   *  @param q   the current value function (like a Q-table)
   *  @param s_t the previous concrete state 
   *  @param a_t the previously decided next action to execute 
   *  @param g_t the accumulated reward (with discounting)
   *  @param γ_t the accumulated discount factor
   *  @return the next state, the next action to execute, the
   *          accumulated reward value, and the accumulated discount 
   *          factor.
   */
  def sem (est: Est) (q_t: VF) 
    (s_t: State, a_t: Action, g_t: Reward, γ_t: Double)
    : Scheduler[(State, Action, Reward, Double)]= est match 

    case Sample (γ) => 
      for 
        (s_tt, r_tt) <- agent.step (s_t) (a_t)
        a_tt         <- vf.chooseAction (epsilon) (q_t) (agent.observe (s_tt))
        g_tt         =  g_t + γ_t * r_tt
        γ_tt         =  γ_t * γ
      yield (s_tt, a_tt, g_tt, γ_tt)

    case Expectation (γ) => 
      for 
        (s_tt, r_tt) <- agent.step (s_t) (a_t)
        os_tt        =  agent.observe (s_tt)
        a_tt         <- vf.chooseAction (epsilon) (q_t) (os_tt)
        expectation  =  allActions
                         .filter { _ != a_tt }
                         .map { a => 
                           vf.probability (epsilon) (q_t) (os_tt, a) * q_t (os_tt, a) }
                         .arithSum
        g_tt         =  g_t + γ_t * (r_tt + expectation) 
        γ_tt         =  γ_t * γ * vf.probability (epsilon) (q_t) (os_tt, a_tt)
      yield (s_tt, a_tt, g_tt, γ_tt)
