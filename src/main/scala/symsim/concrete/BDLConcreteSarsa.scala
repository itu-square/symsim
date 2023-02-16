package symsim
package concrete

import Est.* 

case class BDLConcreteSarsa[State, ObservableState, Action] (
  val agent: Agent[State, ObservableState, Action, Double, Randomized],
  val alpha: Double,
  val gamma: Double,
  val epsilon: Probability,
  val episodes: Int,
) extends BDLLearn[State, ObservableState, Action]:

  val bdl = Update (Nil, alpha, Sample(gamma))

