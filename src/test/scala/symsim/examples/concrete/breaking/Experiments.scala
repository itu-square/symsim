package symsim
package examples.concrete.breaking

class Experiments
  extends ExperimentSpec[CarState, CarObservableState, CarAction]:

  // Import evidence that states and actions can be enumerated
  import Car.*

  val sarsa = symsim.concrete.ConcreteSarsa (
     agent = Car,
     alpha = 0.1,
     gamma = 0.1,
     epsilon = 0.05,
     episodes = 100000,
  )

  s"Breaking Car experiment with $sarsa" in {
    val policy = learnAndLog (sarsa)
  }
