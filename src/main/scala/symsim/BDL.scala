package symsim
package concrete

import cats.Foldable
import cats.syntax.foldable.*
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
case class BDLLearn[State, ObservableState, Action] (
  val agent: Agent[State, ObservableState, Action, Double, Randomized],
  val bdl: Update,
  val epsilon: Probability, 
  val episodes: Int
) extends ConcreteExactRL[State, ObservableState, Action], 
  ConcreteQTable[State, ObservableState, Action]:
  
  override def alpha: Double = bdl.alpha

  override def toString: String =
    s"BDL(..., 𝜀=$epsilon, $episodes episodes)"

  // import agent.instances.given

  /** A single step of the learning algorithm
   *
   *  @param q   the last Q-matrix
   *  @param s_t current state
   *  @return the updated matrix Q, the successor state, and a
   *          reward difference (the size of the update performed)
   */
  def learningEpoch (q: VF, s_t: State, a_t: Action)
    : Randomized[(VF, State, Action)] =
    for
      sa_tt <- agent.step (s_t) (a_t) // TODO, dummy to compile
      // (s_tt, r_tt) = sa_tt
      // // SARSA: on-policy (p.844 in Russel & Norvig)
      // a_tt <- chooseAction (q) (agent.observe (s_tt))

      // ds_t = agent.observe (s_t)
      // ds_tt = agent.observe (s_tt)
      // old_entry = q (ds_t, a_t)
      // correction = r_tt + gamma * q (ds_tt, a_tt) - old_entry
      // qval = old_entry + alpha * correction

      // q1 = q.updated (ds_t, a_t, qval)
    yield ??? // (q1, s_tt, a_tt)

  /** Semantics of a sequence of estimation steps. */
  def sem (ests: List[Est]) (q_t: VF) 
    (s_t: State, a_t: Action, g_t: Double, γ_t: Double)
    : Randomized[(State, Action, Double, Double)]= 

    ests.foldM[Randomized, (State, Action, Double, Double)]
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
    (s_t: State, a_t: Action, g_t: Double, γ_t: Double)
    : Randomized[(State, Action, Double, Double)]= est match 

    case Sample (γ) => 
      for 
        sr_tt        <- agent.step (s_t) (a_t)
        (s_tt, r_tt) = sr_tt
        a_tt         <- chooseAction (q_t) (agent.observe (s_tt))
        g_tt         = g_t + γ_t * r_tt
        γ_tt         = γ_t * γ
      yield (s_tt, a_tt, g_tt, γ_tt)

    case Expectation (γ) => 
      for 
        sr_tt        <- agent.step (s_t) (a_t)
        (s_tt, r_tt) = sr_tt
        os_tt        = agent.observe (s_tt)
        a_tt         <- chooseAction (q_t) (os_tt)
        expectation  = agent.instances.allActions
                       .filter { _ != a_tt }
                       .map { a => 
                         probability (q_t) (os_tt, a) * value (q_t) (os_tt, a) }
                       .sum
        g_tt         = g_t + γ_t * (r_tt + expectation) 
        γ_tt         = γ_t * γ * probability (q_t) (os_tt, a_tt)
      yield (s_tt, a_tt, g_tt, γ_tt)
