package symsim
package concrete

import cats.kernel.BoundedEnumerable

case class BdlConcreteExpectedSarsa [
  State, 
  ObservableState: BoundedEnumerable,
  Action: BoundedEnumerable
] (
  val agent: Agent[State, ObservableState, Action, Double, Randomized],
  val alpha: Double,
  val gamma: Double,
  val epsilon: Probability,
  val episodes: Int,
) extends BdlLearn[State, ObservableState, Action, Double, Randomized],
  ConcreteExactRL[State, ObservableState, Action]:

  import Est.*  
  val bdl = Update (List(Sample (gamma)), alpha, Expectation (gamma))
