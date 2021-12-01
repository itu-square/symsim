package symsim
package examples.concrete.windygrid

class WindyGridExperiments
   extends ExperimentSpec[GridState, GridFiniteState, GridAction.Value]:

   // Import evidence that states and actions can be enumerated
   import WindyGrid._

   val sarsa = symsim.concrete.ConcreteSarsa (
     agent = WindyGrid,
     alpha = 0.1,
     gamma = 0.1,
     epsilon = 0.1,
     episodes = 0
   )

   s"WindyGrid experiment with $sarsa" in {
     val policy = learnAndLog (sarsa)
   }
