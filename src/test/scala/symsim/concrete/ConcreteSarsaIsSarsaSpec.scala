package symsim
package concrete

import _root_.symsim.examples.concrete.mountaincar.{MountainCar, CarState, CarFiniteState, CarAction, CarReward}
import _root_.symsim.laws.discipline.SarsaTests

class ConcreteSarsaIsSarsaSpec extends SymSimSpec:

  val csarsa = ConcreteSarsa[
    CarState,
    CarFiniteState,
    CarAction
  ] (
    agent = MountainCar,
    alpha = 0.1,
    gamma = 1.0,
    epsilon = 0.05,
    epochs = 5000,
    seed = 1000
  )

  checkAll ("concrete.ConcreteSarsa is Sarsa",
    new SarsaTests[CarState, CarFiniteState, CarAction, CarReward, Randomized] (csarsa)
      .sarsa )
