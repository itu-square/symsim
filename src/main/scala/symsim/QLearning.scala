package symsim

import cats.syntax.flatMap.*
import cats.syntax.functor.*

import symsim.Arith.*

trait QLearning[State, FiniteState, Action, Reward, Scheduler[_]]
   extends ExactRL[State, FiniteState, Action, Reward, Scheduler]:

   import agent.instances.given

   /** A single step of the learning algorithm
     *
     * @param q the last Q-matrix
     * @param s_t current state
     * @return the updated matrix Q, the successor state, and a
     * reward difference (the size of the update performed)
     */
   override def learningEpoch (q: Q, s_t: State): Scheduler[(Q, State)] = for
      a_t <- chooseAction (q) (s_t)
      sa_tt <- agent.step (s_t) (a_t)
      (s_tt, r_tt) = sa_tt
      a_tt = bestAction (q) (s_tt)

      // this is Q-learning not SARSA (p.844 in Russel & Norvig)
      ds_t = agent.discretize (s_t)
      ds_tt = agent.discretize (s_tt)
      old_entry = q (ds_t) (a_t)
      correction = r_tt + gamma * q (ds_tt) (a_tt) - old_entry
      qval = old_entry + alpha * correction

      q1 = q + (ds_t -> (q (ds_t) + (a_t -> qval)))
   yield (q1, s_tt)