package symsim
package concrete

import cats.kernel.BoundedEnumerable

case class BdlConcreteSarsa [
  State, 
  ObservableState: BoundedEnumerable,
  Action: BoundedEnumerable
] (
  val agent: Agent[State, ObservableState, Action, Double, Randomized2],
  val alpha: Double,
  val gamma: Double,
  val epsilon0: Probability,
  val episodes: Int,
) extends BdlLearn[State, ObservableState, Action, Double, Randomized2],
  ConcreteExactRL[State, ObservableState, Action],
  NoDecay:

  import Est.*, Upd.*  
  val bdl = Update (List(Sample (gamma)), alpha, SampleU)
