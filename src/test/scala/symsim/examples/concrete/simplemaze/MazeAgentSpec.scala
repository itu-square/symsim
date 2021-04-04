Package symsim
package examples.concrete.simplemaze

class MazeAgentSpec extends SymSimSpec {

  checkAll( "concrete.simplemaze.Maze is an Agent",

    new laws.discipline.AgentTests[
      MazeState,
      MazeFiniteState,
      MazeAction,
      MazeReward,
      concrete.Randomized
    ].agent (Maze)

  )

}
