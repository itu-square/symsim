package symsim
package examples.concrete.multiAgentMaze

import MAMaze.instances.given

class SarsaExperiments
  extends ExperimentSpec[MAMazeState, MAMazeState, MAMazeAction]:

  val sarsa = symsim.concrete.ConcreteSarsa (
    agent = MAMaze,
    alpha = 0.1,
    gamma = 1,
    epsilon = 0.05,
    episodes = 10000,
  )

  s"MultiAgentMaze experiment with ${sarsa}" in {
    val policy = learnAndLog (sarsa)
  }
