package symsim
package concrete

import _root_.symsim.examples.concrete.breaking.{Car, CarState, CarFiniteState, CarAction, CarReward}
import _root_.symsim.laws.discipline.SarsaTests

class ConcreteSarsaIsSarsaSpec extends SymSimSpec {

    // TODO: This would be nice to pbt
    val csarsa = ConcreteSarsa[
      CarState,
      CarFiniteState,
      CarAction
    ] (
      agent = Car,
      alpha = 0.1,
      gamma = 0.1,
      distraction = 0.05,
      epochs = 5000,
      seed = 1000
    )

  checkAll ("concrete.ConcreteSarsa is Sarsa",
    SarsaTests[CarState, CarFiniteState, CarAction, CarReward, Randomized].sarsa (csarsa) )

}
