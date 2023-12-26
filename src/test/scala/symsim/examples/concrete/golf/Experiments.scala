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
    epsilon = 0.1,
    episodes = 20000,
  )

  s"Golf experiment with ${sarsa}" in {
    val policies = learnAndLog(sarsa)
    val samplePolicies = policies.grouped(10).take(10).flatMap(_.headOption).toList
    evalAndLog(sarsa, samplePolicies, "golf.csv")
  }
