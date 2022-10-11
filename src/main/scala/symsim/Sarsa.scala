package symsim

import cats.syntax.flatMap.*
import cats.syntax.functor.*

import symsim.Arith.*

trait Sarsa[State, ObservableState, Action, Reward, Scheduler[_]]
   extends ExactRL[State, ObservableState, Action, Reward, Scheduler],
           QTable[State, ObservableState, Action, Reward, Scheduler]:

   import agent.instances.given

   /** A single step of the learning algorithm
     *
     * @param q the last Q-matrix
     * @param s_t current state
     * @return the updated matrix Q, the successor state, and a
     * reward difference (the size of the update performed)
     */
   override def learningEpoch (q: VF, s_t: State, a_t: Action)
      : Scheduler[(VF, State, Action)] =
      for
         sa_tt <- agent.step (s_t) (a_t)
         (s_tt, r_tt) = sa_tt
         // SARSA: on-policy (p.844 in Russel & Norvig)
         a_tt <- chooseAction (q) (agent.discretize (s_tt))

         ds_t = agent.discretize (s_t)
         ds_tt = agent.discretize (s_tt)
         old_entry = q (ds_t) (a_t)
         correction = r_tt + gamma * q (ds_tt) (a_tt) - old_entry
         qval = old_entry + alpha * correction

         q1 = q + (ds_t -> (q (ds_t) + (a_t -> qval)))
      yield (q1, s_tt, a_tt)
