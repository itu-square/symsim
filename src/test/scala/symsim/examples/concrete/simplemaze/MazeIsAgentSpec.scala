package symsim
package examples.concrete.simplemaze

import laws.AgentLaws
import laws.EpisodicLaws

private val maze = new Maze (using spire.random.rng.SecureJava.apply)

class MazeIsAgentSpec
  extends SymSimSpec:

  checkAll ("concrete.simplemaze.Maze is an Agent", AgentLaws (maze).laws)
  checkAll ("concrete.simplemaze.Maze is Episodic", EpisodicLaws (maze).laws)
