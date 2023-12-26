package symsim
package examples.concrete.braking

import symsim.concrete.Randomized

class Experiments
  extends ExperimentSpec[CarState, CarObservableState, CarAction]:

  // Import evidence that states and actions can be enumerated
  import Car.*
  import Car.instances.given

  val sarsa = symsim.concrete.ConcreteSarsa (
    agent = Car,
    alpha = 0.1,
    gamma = 0.1,
    epsilon = 0.05,
    episodes = 100000,
  )

  s"Braking Car experiment with $sarsa" in {
    val policies = learnAndLog (sarsa)
      .grouped (10)
      .take (10)
      .flatMap { _.headOption }
      .toList
    // FIXME: I tentatively just try from an adhoc state
    val evalInitial = Randomized.const (CarState (10.0, 0.0))
    eval (sarsa, policies, Some (evalInitial))
      .save ("braking.csv")
  }
