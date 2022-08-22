package symsim
package examples.concrete.mountaincar

class Experiments
   extends ExperimentSpec[CarState, CarObservableState, CarAction]:

   // Import evidence that states and actions can be enumerated
   import MountainCar._

   val sarsa = symsim.concrete.ConcreteSarsa (
     agent = MountainCar,
     alpha = 0.1,
     gamma = 0.1,
     epsilon = 0.05,
     episodes = 0,
   )

   s"MountainCar experiment with $sarsa" in {
     val policy = learnAndLog (sarsa)
   }
