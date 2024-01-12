package symsim
package concrete

private val unitAgent = 
  new UnitAgent (using spire.random.rng.SecureJava.apply)
import unitAgent.instances.enumAction

class UnitAgentExperiments
  extends ExperimentSpec[UnitState, UnitState, UnitAction]:

  val sarsa = symsim.concrete.ConcreteSarsa (
     agent = unitAgent,
     alpha = 0.1,
     gamma = 0.1,
     epsilon0 = 0.05, // explore vs exploit ratio
     episodes = 100,
  )

  s"UnitAgent test run with $sarsa (should not crash)" in {
    val p: sarsa.Policy = learnAndLog (sarsa).head
    (p.isEmpty || p (()) == ()) should be (true)
  }

  val qLearning = symsim.concrete.ConcreteQLearning (
     agent = unitAgent,
     alpha = 0.1,
     gamma = 0.1,
     epsilon0 = 0.05, // explore vs exploit ratio
     episodes = 100,
  )

  s"UnitAgent test run with $qLearning (should not crash)" in {
    val p: qLearning.Policy = learnAndLog (qLearning).head
    (p.isEmpty || p(()) == ()) should be (true)
  }
