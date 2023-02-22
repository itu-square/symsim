package symsim
package concrete

import cats.kernel.BoundedEnumerable
import cats.syntax.option.*

import org.typelevel.paiges.Doc
import symsim.QTable

class ConcreteQTable[ObservableState: BoundedEnumerable, Action: BoundedEnumerable]
  extends QTable[ObservableState, Action, Double, Randomized]:

  def value (q: Q) (s: ObservableState, a: Action): Double = 
    q (s, a)

  def probability (ε: Probability) (q: Q) (s: ObservableState, a: Action): Double = 
    if a == bestAction (q) (s) 
    then 1 - ε + ε / allActions.size
    else  ε / allActions.size


  def bestAction (q: Q) (s: ObservableState): Action =
    val qs = q.actionValues (s).map { _.swap }
    if qs.isEmpty 
      then allActions.head
      else qs (qs.keys.max)


  def chooseAction (ε: Probability) (q: Q) (s: ObservableState)
    : Randomized[Action] = for
      explore <- Randomized.coin (ε)
      action  <- if explore
                 then Randomized.oneOf (allActions*)
                 else Randomized.const (bestAction (q) (s))
    yield action


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
