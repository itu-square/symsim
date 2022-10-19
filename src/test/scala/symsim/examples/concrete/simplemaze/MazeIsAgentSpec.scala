package symsim
package examples.concrete.simplemaze

import laws.AgentLaws
import laws.EpisodicLaws

class MazeIsAgentSpec
  extends SymSimSpec:

  implicit override val generatorDrivenConfig =
    PropertyCheckConfiguration(minSuccessful = 100)

  checkAll ("concrete.simplemaze.Maze is an Agent", AgentLaws (Maze).laws)
  checkAll ("concrete.simplemaze.Maze is Episodic", EpisodicLaws (Maze).laws)
