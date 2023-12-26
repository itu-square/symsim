package symsim
package examples.concrete.cartpole

// Import evidence that states and actions can be enumerated
import CartPole.instances.given

class Experiments extends 
  ExperimentSpec[CartPoleState, CartPoleObservableState, CartPoleAction]:

  val sarsa = symsim.concrete.ConcreteSarsa (
    agent = CartPole,
    alpha = 0.1,
    gamma = 0.1,
    epsilon = 0.05,
    episodes = 20000,
  )

  s"CartPole experiment with $sarsa" in {
    val policies = learnAndLog (sarsa)
      .grouped (10)
      .take (10)
      .flatMap { _.headOption }
      .toList
    eval (sarsa, policies)
      .save ("cartpole.csv")
  }
