package symsim
package examples.concrete.braking

import symsim.concrete.Randomized2

private given spire.random.rng.SecureJava = 
  spire.random.rng.SecureJava.apply
private val car: Car = new Car
import car.instances.{enumAction, enumState}

class Experiments
  extends ExperimentSpec[CarState, CarObservableState, CarAction]:

  val sarsa = symsim.concrete.ConcreteSarsa (
    agent = car,
    alpha = 0.1,
    gamma = 0.1,
    epsilon0 = 0.05,
    episodes = 100000,
  )

  s"Braking Car experiment with $sarsa" in {
    val policies = learnAndLog (sarsa)
      .grouped (10)
      .take (10)
      .flatMap { _.headOption }
      .toList
    // FIXME: I tentatively just try from an adhoc state
    val evalInitial = Randomized2.const (CarState (10.0, 0.0))
    eval (sarsa, policies, Some (evalInitial))
      .save ("braking.csv")
  }
