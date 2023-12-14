package symsim

import cats.syntax.flatMap.*
import cats.syntax.functor.*

import symsim.Arith.*

trait QLearning[State, ObservableState, Action, Reward, Scheduler[_]]
  extends ExactRL[State, ObservableState, Action, Reward, Scheduler]:

  import agent.instances.given
  import vf.*

  def gamma: Double

  /** A single step of the learning algorithm
    *
    * @param q the last Q-matrix
    * @param s_t current state
    * @return the updated matrix Q, the successor state, and a
    * reward difference (the size of the update performed)
    */
  override def learningEpoch (q: VF, r_t: Reward, s_t: State, a_t: Action)
  : Scheduler[(VF, Reward, State, Action)] =
    for
      sa_tt        <- agent.step (s_t) (a_t)
      (s_tt, r_tt) = sa_tt
      r_acc        = r_tt + r_t

      ds_t         = agent.observe (s_t)
      ds_tt        = agent.observe (s_tt)
      
      // Q-learning is off-policy (p.844 in Russel & Norvig)
      a_tt         = bestAction (q) (ds_tt)
      old_entry    = q (ds_t, a_t)
      correction   = r_tt + gamma * q (ds_tt, a_tt) - old_entry
      qval         = old_entry + alpha * correction

      q1           = q.updated (ds_t, a_t, qval)
      a_tt1        <- chooseAction (ε) (q1) (ds_tt)
    yield (q1, r_acc, s_tt, a_tt1)
