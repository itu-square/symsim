package symsim
package concrete

class UnitAgentExperiments
   extends ExperimentSpec[UnitState, UnitState, UnitAction]:

   // Import evidence that states and actions can be enumerated
   import UnitAgent._

   val sarsa = symsim.concrete.ConcreteSarsa (
      agent = UnitAgent,
      alpha = 0.1,
      gamma = 0.1,
      epsilon = 0.05, // explore vs exploit ratio
      episodes = 100,
   )

   s"UnitAgent test run with $sarsa (should not crash)" in {
      learnAndLog (sarsa) should be (List (() -> ()).toMap)
   }

   val qLearning = symsim.concrete.ConcreteQLearning (
      agent = UnitAgent,
      alpha = 0.1,
      gamma = 0.1,
      epsilon = 0.05, // explore vs exploit ratio
      episodes = 100,
   )

   s"UnitAgent test run with $qLearning (should not crash)" in {
       learnAndLog (qLearning) should be (List (() -> ()).toMap)
   }


