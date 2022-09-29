package symsim

import cats.syntax.flatMap.*
import cats.syntax.functor.*

import symsim.Arith.*

trait ExpectedSarsa[State, ObservableState, Action, Reward, Scheduler[_]]
  extends ExactRL[State, ObservableState, Action, Reward, Scheduler],
    QTable[State, ObservableState, Action, Reward, Scheduler] :

  import agent.instances.given

  /** A single step of the learning algorithm
    *
    * @param q the last Q-matrix
    * @param s_t current state
    * @return the updated matrix Q, the successor state, and a
    * reward difference (the size of the update performed)
    */
  override def learningEpoch (q: Q, s_t: State, a_t: Action)
  : Scheduler[(Q, State, Action)] =
    for
      sa_tt <- agent.step (s_t) (a_t)
      (s_tt, r_tt) = sa_tt
      // Expected Sarsa (p.133 in Sutton & Barto)
      ds_t = agent.discretize (s_t)
      ds_tt = agent.discretize (s_tt)
      old_entry = q (ds_t) (a_t)

      expectedQ = for
        a_tt <- agent.instances.allActions
        q_a_tt = (1.0 / agent.instances.allActions.length) .times[Reward] (q (ds_tt) (a_tt))
      yield q_a_tt

      correction = r_tt + gamma * expectedQ.foldLeft ((0.0).asInstanceOf[Reward])( _ + _ ) - old_entry
      qval = old_entry + alpha * correction
      q1 = q + (ds_t -> (q (ds_t) + (a_t -> qval)))
      a_tt1 <- chooseAction (q1) (s_tt)
    yield (q1, s_tt, a_tt1)