package symsim
package examples.concrete.pumping

class Experiments
  extends ExperimentSpec[PumpState,ObservablePumpState,PumpAction]:

  // Import evidence that states and actions can be enumerated
  import Pump._

  val sarsa = symsim.concrete.ConcreteSarsa (
    agent = Pump,
    alpha = 0.1,
    gamma = 0.9,
    epsilon = 0.05,
    episodes = 100,
  )

  s"Pumping experiment with $sarsa" in {
    val policy = learnAndLog (sarsa, outputToFile = Some("pump"))
  }
