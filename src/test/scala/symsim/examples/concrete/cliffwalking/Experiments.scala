package symsim
package examples.concrete.cliffWalking

import CliffWalking.instances.given

class Experiments
  extends ExperimentSpec[CWState, CWObservableState, CWAction]:

  // Import evidence that states and actions can be enumerated
  import CliffWalking.*

  val sarsa = symsim.concrete.ConcreteSarsa (
    agent = CliffWalking,
    alpha = 0.1,
    gamma = 0.1,
    epsilon0 = 0.1,
    episodes = 100
  )

  s"CliffWalking experiment with $sarsa" in {
    val policies = learnAndLog (sarsa)
      .grouped (10)
      .take (10)
      .flatMap { _.headOption }
      .toList
    eval (sarsa, policies)
      .save ("cliffwalking.csv")
  }
