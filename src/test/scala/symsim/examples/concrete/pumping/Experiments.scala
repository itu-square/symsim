package symsim
package examples.concrete.pumping

private val pump = new Pump (using spire.random.rng.SecureJava.apply)

class Experiments
  extends ExperimentSpec[PumpState, ObservablePumpState, PumpAction]:

  // Import evidence that states and actions can be enumerated
  import pump.*
  import pump.instances.given

  val sarsa = symsim.concrete.ConcreteSarsa (
    agent = pump,
    alpha = 0.1,
    gamma = 0.9,
    epsilon0 = 0.05,
    episodes = 3000,
  )

  s"Pumping experiment with $sarsa" in {
    val policies = learnAndLog (sarsa, outputToFile = Some("pump"))
      .grouped (10)
      .take (10)
      .flatMap { _.headOption }
      .toList
    val results = eval (sarsa, policies)
    results.save ("pumping.csv")
  }
