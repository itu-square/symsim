package symsim
package examples.concrete.golf

class Experiments
  extends ExperimentSpec[GolfState, GolfState, GolfAction]:

  import Golf.*

  val sarsa = symsim.concrete.ConcreteSarsa (
    agent = Golf,
    alpha = 0.1,
    gamma = 0.1,
    epsilon = 0.1,
    episodes = 20000,
  )

  s"Golf experiment with ${sarsa}" in { val policy = learnAndLog (sarsa) }
