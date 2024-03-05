package symsim
package examples.concrete.simplemaze

private val maze = new Maze (using spire.random.rng.SecureJava.apply)
import maze.instances.{enumAction, enumState}

import MazeAction.*

class ExpectedSarsaExperiments
   extends ExperimentSpec[MazeState,MazeObservableState,MazeAction]:

   val sarsa = symsim.concrete.ConcreteExpectedSarsa (
     agent = maze,
     alpha = 0.1,
     gamma = 1,
     epsilon0 = 0.05,
     episodes = 60000,
   )

   s"SimpleMaze experiment with ${sarsa}" in {

      val policy = learnAndLog (sarsa).head

      withClue ("1,1") { policy (1, 1) should be (Up) }
      withClue ("1,2") { policy (1, 2) should be (Up) }
      withClue ("1,3") { policy (1, 3) should be (Right) }
      withClue ("2,1") { policy (2, 1) should be (Left) }
      withClue ("2,3") { policy (2, 3) should be (Right) }

      // We leave 4,3 and 4,2 unconstrained (loosing and winning,
      // final states)
      
      // Which of policy is optimal is a bit hard to
      // establish, and depends on the constants in the reward
      // function. We include several options to decrease flakiness of
      // tests (mostly positions in column 3 are sensitive)

      // this appears to be still a good move
      withClue ("3,1") { policy (3,1) should be (Left) }
      // Left is safest but AIAMA reports the dangerous Up
      withClue ("3,2") { policy (3,2) should (be (Left) or be (Up))  }
      // Up is safest but AIAMA reports the somewhat risky Right
      withClue ("3,3") { policy (3,3) should (be (Up) or be (Right)) }
      // Left is faster, down is safer
      withClue ("4,1") { policy (4, 1) should (be (Down) or be (Left)) }
   }
