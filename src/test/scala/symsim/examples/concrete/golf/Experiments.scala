package symsim
package examples.concrete.golf

class Experiments
  extends ExperimentSpec[GolfState, GolfState, GolfAction]:

  import Golf.*
  import Golf.instances.given

  val sarsa = symsim.concrete.ConcreteSarsa (
    agent = Golf,
    alpha = 0.1,
    gamma = 0.1,
    epsilon0 = 0.1,
    episodes = 20000,
  )

  s"Golf experiment with ${sarsa}" in {
    val policies = learnAndLog (sarsa)
      .grouped (10)
      .take (10)
      .flatMap { _.headOption }
      .toList
    eval (sarsa, policies)
      .save ("golf.csv")
  }
