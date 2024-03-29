package symsim
package examples.concrete.windygrid

class Experiments
  extends ExperimentSpec[GridState, GridObservableState, GridAction]:

  // Import evidence that states and actions can be enumerated
  import WindyGrid.*
  import WindyGrid.instances.given

  val sarsa = symsim.concrete.ConcreteSarsa (
    agent = WindyGrid,
    alpha = 0.1,
    gamma = 0.1,
    epsilon0 = 0.1,
    episodes = 10000
  )

  s"WindyGrid experiment with $sarsa" in {
    val policies = learnAndLog (sarsa)
      .grouped (10)
      .take (10)
      .flatMap { _.headOption }
      .toList
    val results = eval (sarsa, policies)
    results.save ("windygrid.csv")
  }
