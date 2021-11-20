package symsim

import cats.Monad
import cats.syntax.functor.*
import cats.syntax.flatMap.*
import cats.syntax.foldable.*
import cats.syntax.option.*

import org.scalacheck.Gen
import org.typelevel.paiges.Doc

trait ExactRL[State, FiniteState, Action, Reward, Scheduler[_]]
   extends RL[FiniteState, Action]:

   import agent.instances.given

   type Q = Map[FiniteState, Map[Action, Reward]]

   val agent: Agent[State, FiniteState, Action, Reward, Scheduler]

   def alpha: Double
   def gamma: Double

   def bestAction (q: Q) (s: State): Action

   def chooseAction (q: Q) (s: State): Scheduler[Action]

   /** A single step of the learning algorithm
     *
     * @param q the last Q-matrix
     * @param s_t current state
     * @return the updated matrix Q, the successor state, and a
     * reward difference (the size of the update performed)
     *
     * To acommodate Q-Learning and SARSA in a single design, we receive
     * the action a_t to the iteration, and we also produce the action a_t
     * for the next iteration.  This allows SARSA to pass over the 'lookahead'
     * to the next iteration and stay properly on policy.  In Q-Learning this
     * introduces a small presentation complication, not more.
     */
   def learningEpoch (q: Q, s_t: State, a_t: Action): Scheduler[(Q, State, Action)]

   /** Execute a full learning episode (until the final state of agent is
     * reached).
     */
   def learningEpisode (q: Q, s_t: State): Scheduler[Q] =
      def p (q: Q, s: State, a: Action): Boolean =
         agent.isFinal (s)
      for
         a <- chooseAction (q) (s_t)
         fin <- Monad[Scheduler].iterateUntilM (q, s_t, a) (learningEpoch) (p)
      yield fin._1

   /** Construct a zero initialized Q matrix */
   def initQ: Q

   /** Executes as many full learning episodes (until the final state of agent is
     * reached) as the given state scheduler generates.  For this method to work
     * the scheduler needs to be foldable, and we use foldRight with Eval, to
     * make the evaluation lazy. We force the evaluation when we
     * are done to return the value.  However, to my best understanding, if the
     * Scheduler is lazy then the evaluation is not really doing more than just
     * formulating the thunk of that scheduler.
     */
   final def learn (q: Q, ss: => Scheduler[State]): Scheduler[Q] =
      ss.foldM[Scheduler,Q] (q) (learningEpisode)

   /** Convert the matrix Q after training into a Policy map. TODO: should not
     * this be using the bestAction method? Or, why is the best action method
     * abstract? Or is qToPolicy too concrete to be here?
     */
   def qToPolicy (q: Q) (using Ordering[Reward]): Policy =
     def best (m: Map[Action,Reward]): Action =
       m.map { _.swap } (m.values.max)
     q.view.mapValues (best).to (Map)


   /** Generate total Q matrices for testing. */
   val genQ: Gen[Q] =
      val as = agent.instances.allActions
      val genReward = agent.instances.arbitraryReward.arbitrary
      val genActionReward: Gen[Map[Action,Reward]] = for
        // TODO refactor, seek what is available for maps
        rewards <- Gen.sequence[List[Reward], Reward]
          { List.fill (as.size) (genReward) }
        ars = as zip rewards
      yield Map (ars: _*)

      val fs = agent.instances.allFiniteStates
      val genStateActionRewards: Gen[Q] = for
        // TODO refactor, seek what is available for maps
        mars <- Gen.sequence[List[Map[Action,Reward]], Map[Action,Reward]]
          { List.fill (fs.size) (genActionReward) }
        smars = fs zip mars
      yield Map (smars: _*)

      genStateActionRewards



   /** We assume that all values define the same set of actions valuations.  */
   def pp_Q (q: Q): Doc =
      val headings = "" ::q
         .values
         .head
         .keys
         .map (_.toString)
         .toList
         .sorted
      def fmt (s: FiniteState, m: Map[Action,Reward]): List[String] =
         s.toString ::m
            .toList
            .sortBy (_._1.toString)
            .map { _._2.toString.take (7).padTo (7,'0') }
      val rows = q
         .toList
         .sortBy (_._1.toString)
         .map (fmt)
      symsim.tabulate (' ', " | ", headings ::rows, "-".some, "-+-".some)
