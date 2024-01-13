package symsim
package examples.concrete.cartpole

private given spire.random.rng.SecureJava = 
  spire.random.rng.SecureJava.apply
private val cartPole: CartPole = new CartPole 
import cartPole.instances.{enumAction, enumState}

class Experiments extends 
  ExperimentSpec[CartPoleState, CartPoleObservableState, CartPoleAction]:

  val sarsa = symsim.concrete.ConcreteSarsa (
    agent = cartPole,
    alpha = 0.1,
    gamma = 0.1,
    epsilon0 = 0.05,
    episodes = 2000,
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
