package symsim
package examples.concrete.cliffwalking

class Experiments
  extends ExperimentSpec[CWState, CWObservableState, CWAction]:

  // Import evidence that states and actions can be enumerated
  import CliffWalking.*

  val sarsa = symsim.concrete.ConcreteSarsa (
    agent = CliffWalking,
    alpha = 0.1,
    gamma = 0.1,
    epsilon = 0.1,
    episodes = 400
  )

  s"CliffWalking experiment with $sarsa" in {
    val policy = learnAndLog (sarsa)
  }
