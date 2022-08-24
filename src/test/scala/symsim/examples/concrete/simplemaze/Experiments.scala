package symsim
package examples.concrete.simplemaze

class Experiments
   extends ExperimentSpec[MazeState,MazeState,MazeAction]:

   val sarsa = symsim.concrete.ConcreteSarsa (
     agent = Maze,
     alpha = 0.1,
     gamma = 1.0,
     epsilon = 0.05,
     episodes = 30000,
   )

   s"SimpleMaze experiment with ${sarsa}" in {

      val policy = learnAndLog (sarsa)

      val groundTruth = List (
         (1,1) -> Up,
         (1,2) -> Up,
         (1,3) -> Right,
         (2,1) -> Left,
         (2,3) -> Right,
         (3,1) -> Left,
         (3,2) -> Up,
         (3,3) -> Right,
         (4,1) -> Left,
         (4,2) -> Right,
         (4,3) -> Right,
      ).toMap

      policy should be (groundTruth)
   }
