package symsim
package concrete

import Est.* 

case class BdlConcreteSarsa[State, ObservableState, Action] (
  val agent: Agent[State, ObservableState, Action, Double, Randomized],
  val alpha: Double,
  val gamma: Double,
  val epsilon: Probability,
  val episodes: Int,
) extends BdlLearn[State, ObservableState, Action, Double, Randomized],
  ConcreteExactRL[State, ObservableState, Action],
  ConcreteQTable[State, ObservableState, Action]:

  val bdl = Update (Nil, alpha, Sample(gamma))
