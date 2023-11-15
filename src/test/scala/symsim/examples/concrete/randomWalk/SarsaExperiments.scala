package symsim
package examples.concrete.randomWalk

import RandomWalk.instances.given

class SarsaExperiments
   extends ExperimentSpec[RandomWalkState, RandomWalkObservableState, RandomWalkAction]:

   val sarsa = symsim.concrete.ConcreteSarsa (
     agent = RandomWalk,
     alpha = 0.1,
     gamma = 1,
     epsilon = 0.05,
     episodes = 10000,
   )

   s"RandomWalk experiment with ${sarsa}" in {
     val policy = learnAndLog (sarsa)
   }
