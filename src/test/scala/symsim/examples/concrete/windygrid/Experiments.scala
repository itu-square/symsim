package symsim
package examples.concrete.windygrid

private val windyGrid = new WindyGrid (using spire.random.rng.SecureJava.apply)

class Experiments
  extends ExperimentSpec[GridState, GridObservableState, GridAction]:

  // Import evidence that states and actions can be enumerated
  import windyGrid.*
  import windyGrid.instances.given

  val sarsa = symsim.concrete.ConcreteSarsa (
    agent = windyGrid,
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
