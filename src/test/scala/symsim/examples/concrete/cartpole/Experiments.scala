package symsim
package examples.concrete.cartpole

class Experiments
  extends ExperimentSpec [CartPoleState, CartPoleObservableState, CartPoleAction]:

  // Import evidence that states and actions can be enumerated
  import CartPole.*

  val sarsa = symsim.concrete.ConcreteSarsa (
     agent = CartPole,
     alpha = 0.1,
     gamma = 0.1,
     epsilon = 0.05,
     episodes = 20000,
  )

  s"CartPole experiment with $sarsa" in {
    val policy = learnAndLog (sarsa)
  }
