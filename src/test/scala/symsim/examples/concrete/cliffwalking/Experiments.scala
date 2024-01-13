package symsim
package examples.concrete.cliffWalking


private given spire.random.rng.SecureJava = 
  spire.random.rng.SecureJava.apply
private val cliffWalking: CliffWalking = new CliffWalking 
import cliffWalking.instances.{enumAction, enumState}

class Experiments
  extends ExperimentSpec[CWState, CWObservableState, CWAction]:

  // Import evidence that states and actions can be enumerated
  import cliffWalking.*

  val sarsa = symsim.concrete.ConcreteSarsa (
    agent = cliffWalking,
    alpha = 0.1,
    gamma = 0.1,
    epsilon0 = 0.1,
    episodes = 1000
  )

  s"CliffWalking experiment with $sarsa" in {
    val policies = learnAndLog (sarsa)
      .grouped (100)
      .take (10)
      .flatMap { _.headOption }
      .toList
    val fileN = "cliffwalking.csv"
    info (s"Starting evaluation and saving to $fileN")
    eval (sarsa, policies)
      .save (fileN)
  }
