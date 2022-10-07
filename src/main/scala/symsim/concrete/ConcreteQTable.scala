package symsim
package concrete

import cats.syntax.option.*
import org.typelevel.paiges.Doc
import symsim.QTable

trait ConcreteQTable[State, ObservableState, Action]
  extends QTable[State, ObservableState, Action, Double, Randomized]:
  this: ConcreteExactRL[State, ObservableState, Action] =>

  import agent.instances.*

  def bestAction (q: Q) (s: State): Action =
    val qs = q (agent.discretize (s)) map { _.swap }
    qs (qs.keys.max)


  def chooseAction (q: Q) (s: State): Randomized[Action] =
    for
      explore <- Randomized.coin (this.epsilon)
      action <- if explore
      then Randomized.oneOf (allActions*)
      else Randomized.const (bestAction (q) (s))
    yield action


  def runQ: Q =
    val initials = Randomized.repeat (agent.initialize).take (episodes)
    val schedule = learn (this.initialize, initials)
    schedule.head

  override def run: Policy =
    qToPolicy (this.runQ)


  /** Convert the matrix Q after training into a Policy map. TODO: should not
    * this be using the bestAction method? Or, why is the best action method
    * abstract? Or is qToPolicy too concrete to be here?
    */
  def qToPolicy (q: Q) (using Ordering[Double]): Policy =
    def best (m: Map[Action, Double]): Action =
      m.map { _.swap } (m.values.max)
    q.view.mapValues (best).to (Map)


  /** We assume that all values define the same set of actions valuations.  */
  def pp_Q (q: Q): Doc =
    val headings = "" ::q
      .values
      .head
      .keys
      .map (_.toString)
      .toList
      .sorted
    def fmt (s: ObservableState, m: Map[Action, Double]): List[String] =
      s.toString ::m
        .toList
        .sortBy (_._1.toString)
        .map { _._2.toString.take (7).padTo (7,'0') }
    val rows = q
        .toList
        .sortBy (_._1.toString)
        .map (fmt)
    symsim.tabulate (' ', " | ", headings ::rows, "-".some, "-+-".some)
