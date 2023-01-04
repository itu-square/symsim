package symsim
package concrete

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
    : Randomized[(VF, State, Action)] = ???
    for
      // sa_tt <- agent.step (s_t) (a_t)
      // (s_tt, r_tt) = sa_tt
      // // SARSA: on-policy (p.844 in Russel & Norvig)
      // a_tt <- chooseAction (q) (agent.observe (s_tt))

      // ds_t = agent.observe (s_t)
      // ds_tt = agent.observe (s_tt)
      // old_entry = q (ds_t, a_t)
      // correction = r_tt + gamma * q (ds_tt, a_tt) - old_entry
      // qval = old_entry + alpha * correction

      // q1 = q.updated (ds_t, a_t, qval)
    yield (q1, s_tt, a_tt)


    def est (est: List[Est]) (q: VF) (s_t: State, a_t: Action, 0?, 1?): ??? = ???
