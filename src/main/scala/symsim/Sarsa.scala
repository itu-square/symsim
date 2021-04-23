package symsim

import cats.Monad
import cats.MonoidK
import cats.data.Kleisli
import cats.syntax.functor._
import cats.syntax.flatMap._
import cats.syntax.foldable._
import cats.instances.lazyList._

import Arith.arithOps
import Arith.doubleOps
import cats.kernel.BoundedEnumerable
import org.scalacheck.Gen
import org.typelevel.paiges.Doc

// TODO: right now the Action type is a dead type, and all algorithms are just
// sampling across all values (as the action shall be enumarable).  Instead of
// requiring actions to be flatly enumerable, we could have it be enumerable via
// a State Monad, so that the agent does not try obviously invalid actions in
// some state.  This could lead to a performance improvement.
trait Sarsa[State, FiniteState, Action, Reward, Scheduler[_]]
  extends RL[FiniteState, Action] {

  type A = Agent[State, FiniteState, Action, Reward, Scheduler]

  val agent: A

  import agent.instances._

  type Q = Map[FiniteState, Map[Action, Reward]]

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
  def learn1 (q: Q, s_t: State): Scheduler[(Q,State)] = for {

    a_t <- chooseAction (q) (s_t)

    (s_tt, r_tt) = agent.step (s_t) (a_t)

    a_tt = bestAction (q) (s_tt)

    // concerned that this is Q-learning not SARSA (p.844 in Russel & Norvig)
    ds_t = agent.discretize (s_t)
    ds_tt = agent.discretize (s_tt)
    old_entry = q (ds_t) (a_t)
    correction = r_tt + gamma * q (ds_tt) (a_tt) - old_entry
    qval = old_entry + alpha * correction

    q1 = q + (ds_t -> (q (ds_t) + (a_t -> qval)))

  } yield (q1, s_tt)



  /** Execute a full learning episode (until the final state of agent is
    * reached).
    */
  def learn (q: Q, s_t: State): Scheduler[Q] = {
    val initial = q -> s_t
    val f = (learn1 _).tupled
    val p = { (qs: (Q,State)) => agent.isFinal (qs._2) }
    Monad[Scheduler]
      .iterateUntilM[(Q,State)] (initial) (f) (p)
      .map { _._1 }
  }



  /** Execute a full  learning episode from initial  state (until the
    * final state of agent is reached).
    */
  def learn (q: Q): Scheduler[Q] =

    for {
      s0 <- agent.initialize
      result <- learn (q, s0)
    } yield  result


  /** Construct a zero initialized Q matrix */
  def initQ: Q


  /**
   * Execute 'n' full learning episodes (until the final state of agent is
   * reached), starting with the matrix q
   */
  def learnN (n: Int, q: Q) (implicit ar: Arith[Reward]): Scheduler[Q] = {

    // The endomonoid for Kleisli[Scheduler,QS,QS], apparently not automatic
    type EndoKleisli[A] = Kleisli[Scheduler,A,A]
    def endoKleisli[A] (f: A => Scheduler[A]) = Kleisli[Scheduler,A,A] (f)
    implicit val monoid: MonoidK[EndoKleisli] = Kleisli.endoMonoidK[Scheduler]

    val l: Q => Scheduler[Q] = learn _

    val q1 = LazyList
      // Prepare 'epoch'-many learning steps
      .fill (n) (l)
      // Wrap each in Kleisli[Q => Randomized[Q]} and compose them in the monoid
      .foldMapK[EndoKleisli, Q] (endoKleisli[Q])
      // Got a single Kleisli[Q => Randomized[Q]], run it from initial conditions
      .run (q)

    q1

  }



  /** Convert the matrix Q after training into a Policy map */
  def qToPolicy (q: Q) (implicit order: Ordering[Reward]): Policy = {

    def best (m: Map[Action,Reward]): Action =
      m.map { _.swap } (m.values.max)

    q.view.mapValues (best).to (Map)
  }

  /** Generate total Q matrices for testing. */
  val genQ: Gen[Q] = {

    val as = agent.instances.allActions
    val genReward = agent.instances.arbitraryReward.arbitrary

    val genActionReward: Gen[Map[Action,Reward]] =
      for {
        // TODO refactor, seek what is available for maps
        rewards <- Gen.sequence[List[Reward],Reward]
          { List.fill (as.size) (genReward) }
        ars = as zip rewards
      } yield Map (ars: _*)

    val fs = agent.instances.allFiniteStates
    val genStateActionRewards: Gen[Q] =
      for  {
        // TODO refactor, seek what is available for maps
        mars <- Gen.sequence[List[Map[Action,Reward]], Map[Action,Reward]]
          { List.fill (fs.size) (genActionReward) }
        smars = fs zip mars
      } yield Map (smars: _*)

    genStateActionRewards
  }

  def pp_Q (q: Q): Doc = {
    def fmt (ar: Map[Action, Reward]): Doc =
      Doc.tabulate (' ', " ",
        ar.toList
          .map { case (a,r) =>
            val k = a.toString
            val v = Doc.text (r.toString.take (7).padTo (7, ' ')) + Doc.text (" |")
            k -> v
          }
      )
    val rows = q
      .toIterable
      .map { case (s,ar) => (s.toString, fmt (ar).flatten) }
    Doc.tabulate (' ', "... ", rows)
  }

}
