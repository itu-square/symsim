package symsim
package examples.concrete.golf

private given spire.random.rng.SecureJava = 
  spire.random.rng.SecureJava.apply
private val golf: Golf = new Golf 

import golf.instances.{enumAction, enumState}

class Experiments
  extends ExperimentSpec[GolfState, GolfState, GolfAction]:

  val sarsa = symsim.concrete.ConcreteSarsa (
    agent = golf,
    alpha = 0.1,
    gamma = 0.1,
    epsilon0 = 0.1,
    episodes = 20000,
  )

  s"Golf experiment with ${sarsa}" in {
    val policies = learnAndLog (sarsa)
      .grouped (10)
      .take (10)
      .flatMap { _.headOption }
      .toList
    eval (sarsa, policies)
      .save ("golf.csv")
  }
