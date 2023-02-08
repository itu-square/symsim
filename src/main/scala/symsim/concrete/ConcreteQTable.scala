package symsim
package concrete

import cats.syntax.option.*
import org.typelevel.paiges.Doc
import symsim.QTable

trait ConcreteQTable[State, ObservableState, Action]
  extends QTable[State, ObservableState, Action, Double, Randomized]:
  this: ConcreteExactRL[State, ObservableState, Action] =>

  import agent.instances.*

  def value (q: Q) (s: ObservableState, a: Action): Double = 
    q (s, a)

  def bestAction (q: Q) (s: ObservableState): Action =
    val qs = q.actionValues (s).map { _.swap }
    if qs.isEmpty 
      then agent.instances.allActions.head
      else qs (qs.keys.max)


  def chooseAction (q: Q) (s: ObservableState): Randomized[Action] =
    for
      explore <- Randomized.coin (this.epsilon)
      action  <- if explore
                 then Randomized.oneOf (allActions*)
                 else Randomized.const (bestAction (q) (s))
    yield action


  def runQ: Q =
    val initials = Randomized.repeat (agent.initialize).take (episodes)
    val schedule = learn (this.initialize, initials)
    schedule.head


  override def run: Policy =
    qToPolicy (this.runQ)


  /** Convert the matrix Q after training into a Policy map. */
  def qToPolicy (q: Q) (using Ordering[Double]): Policy =
    q.states.map { s => (s, bestAction (q) (s)) }.to (Map)


  /** We assume that all values define the same set of actions valuations.  */
  def pp_Q (q: Q): Doc =
    val actions = q.actions.toList.sortBy { _.toString }
    val headings = "" :: actions
      .map { _.toString }
      .sorted
    def fmt (s: ObservableState, m: List[Double]): List[String] =
      s.toString ::m
        .map { _.toString.take (7).padTo (7, '0') }
    val rows = q
      .states
      .sortBy { _.toString }
      .map { s => (s, actions.map { q (s, _) }) }
      .map (fmt)
    assert (headings.nonEmpty)
    if rows.nonEmpty then
      symsim.tabulate (' ', " | ", headings ::rows, "-".some, "-+-".some)
    else 
      Doc.text ("The lazy Q table does not contain any states")
