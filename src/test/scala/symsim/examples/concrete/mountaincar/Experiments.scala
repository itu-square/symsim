package symsim
package examples.concrete.mountaincar


private given spire.random.rng.SecureJava = 
  spire.random.rng.SecureJava.apply
private val mountainCar: MountainCar = new MountainCar 
import mountainCar.instances.{enumAction, enumState}

class Experiments
  extends ExperimentSpec[CarState, CarObservableState, CarAction]:

  // Import evidence that states and actions can be enumerated
  import mountainCar.*
  import mountainCar.instances.given

  val sarsa = symsim.concrete.ConcreteSarsa (
    agent = mountainCar,
    alpha = 0.1,
    gamma = 0.1,
    epsilon0 = 0.05,
    episodes = 80000,
  )

  s"MountainCar experiment with $sarsa" in {
    val policies = learnAndLog (sarsa)
      .grouped (10)
      .take (10)
      .flatMap { _.headOption }
      .toList
    val fileN = "mountaincar.csv"
    info (s"Evaluation report will be written to $fileN")
    eval (sarsa, policies)
      .save (fileN)
  }
