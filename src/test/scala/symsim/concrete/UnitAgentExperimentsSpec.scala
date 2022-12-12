package symsim
package concrete

class UnitAgentExperiments
  extends ExperimentSpec[UnitState, UnitState, UnitAction]:

  // Import evidence that states and actions can be enumerated
  import UnitAgent.*

  val sarsa = symsim.concrete.ConcreteSarsa (
     agent = UnitAgent,
     alpha = 0.1,
     gamma = 0.1,
     epsilon = 0.05, // explore vs exploit ratio
     episodes = 100,
  )

  s"UnitAgent test run with $sarsa (should not crash)" in {
    val p: sarsa.Policy = learnAndLog (sarsa) 
    (p.isEmpty || p (()) == ()) should be (true)
  }

  val qLearning = symsim.concrete.ConcreteQLearning (
     agent = UnitAgent,
     alpha = 0.1,
     gamma = 0.1,
     epsilon = 0.05, // explore vs exploit ratio
     episodes = 100,
  )

  s"UnitAgent test run with $qLearning (should not crash)" in {
    val p: qLearning.Policy = learnAndLog (qLearning) 
    (p.isEmpty || p (()) == ()) should be (true)
  }
