package symsim
package examples.concrete.bouncingBall

class Experiments
  extends ExperimentSpec[BallState, BallObservableState, BallAction]:

  // Import evidence that states and actions can be enumerated
  import Ball.*
  import Ball.instances.given

  val sarsa = symsim.concrete.ConcreteSarsa (
     agent = Ball,
     alpha = 0.1,
     gamma = 0.1,
     epsilon = 0.05,
     episodes = 1000,
  )

  s"Breaking Ball experiment with $sarsa" in {
    val policy = learnAndLog (sarsa)
  }
