package symsim

import cats.Monad
import cats.MonoidK
import cats.data.Kleisli
import cats.syntax.monad._
import cats.syntax.functor._
import cats.syntax.flatMap._
import cats.syntax.foldable._
import cats.syntax.option._
import cats.instances.lazyList._

import symsim.Arith._
import cats.kernel.BoundedEnumerable
import org.scalacheck.Gen
import org.typelevel.paiges.Doc


// TODO: right now the Action type is a dead type, and all algorithms are just
// sampling across all values (as the action shall be enumarable).  Instead of
// requiring actions to be flatly enumerable, we could have it be enumerable via
// a State Monad, so that the agent does not try obviously invalid actions in
// some state.  This could lead to a performance improvement.
trait Sarsa[State, FiniteState, Action, Reward, Scheduler[_]]
  extends RL[FiniteState, Action]:

  type Q = Map[FiniteState, Map[Action, Reward]]

  val agent: Agent[State, FiniteState, Action, Reward, Scheduler]

  import agent.instances.given

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
    */
  def learn1 (q: Q, s_t: State): Scheduler[(Q,State)] = for
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


  /** Execute a full learning episode (until the final state of agent is
    * reached).
    */
  def learn (q: Q, s_t: State): Scheduler[Q] =
    val initial = q -> s_t
    val f = (learn1 _).tupled
    def p (q: Q, s: State): Boolean = agent.isFinal (s)
    summon[Monad[Scheduler]]
      .iterateUntilM[(Q,State)] (initial) (f) (p.tupled)
      .map { _._1 }



  /** Execute a full  learning episode from initial  state (until the
    * final state of agent is reached).
    */
  def learn (q: =>Q): Scheduler[Q] =
    agent
      .initialize
      .flatMap { s0 => learn (q, s0) }


  /** Construct a zero initialized Q matrix */
  def initQ: Q

  /** Execute 'n' full learning episodes (until the final state of agent is
    * reached), starting with the matrix q
    */
  final def learnN (n: Int, q: Q) (using ar: Arith[Reward]): Scheduler[Q] =
     def f (q: Q, n: Int): Scheduler[(Q,Int)] =
       learn (q) map { q1 => q1 -> (n-1) }
     def p (q: Q, n: Int): Boolean = n > 0
     summon[Monad[Scheduler]]
        .iterateWhileM[(Q,Int)] ((q, n)) (f.tupled) (p.tupled)
        .map { _._1 }


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
      rewards <- Gen.sequence[List[Reward],Reward]
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
