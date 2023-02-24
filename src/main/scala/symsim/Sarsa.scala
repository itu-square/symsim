package symsim

import cats.syntax.flatMap.*
import cats.syntax.functor.*

import symsim.Arith.*

trait Sarsa[State, ObservableState, Action, Reward, Scheduler[_]]
  extends ExactRL[State, ObservableState, Action, Reward, Scheduler]:

  import vf.*

  import agent.instances.given

  def gamma: Double

  /** A single step of the learning algorithm
    *
    * @param q the last Q-matrix
    * @param s_t current state
    * @return the updated matrix Q, the successor state, and a
    * reward difference (the size of the update performed)
    */
  override def learningEpoch (q_t: VF, s_t: State, a_t: Action)
    : Scheduler[(VF, State, Action)] = for
      sa_tt        <- agent.step (s_t) (a_t)
      (s_tt, r_tt)  = sa_tt
                      // SARSA: on-policy (p.844 in Russel & Norvig)
      os_t          = agent.observe (s_t)
      os_tt         = agent.observe (s_tt)
      a_tt         <- vf.chooseAction (ε) (q_t) (os_tt)
      q_t_value     = q_t (os_t, a_t)
      correction    = r_tt + gamma * q_t (os_tt, a_tt) - q_t_value
      q_tt_value    = q_t_value + alpha * correction
      q_tt          = q_t.updated (os_t, a_t, q_tt_value)
    yield (q_tt, s_tt, a_tt)
