package symsim
package examples.concrete.continuousmaze

import ContinuousMaze.instances.given

class SarsaExperiments
   extends ExperimentSpec[MazeState, MazeState, MazeAction]:

   val sarsa = symsim.concrete.ConcreteSarsa (
     agent = ContinuousMaze,
     alpha = 0.1,
     gamma = 1,
     epsilon = 0.05,
     episodes = 50000,
   )

   s"ContinuousMaze experiment with ${sarsa}" in {
      val policy = learnAndLog (sarsa)
   }
