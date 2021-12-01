package symsim
package examples.concrete.simplemaze

import laws.AgentLaws
import laws.EpisodicLaws

class MazeIsAgentSpec
   extends SymSimSpec:
   checkAll ("concrete.simplemaze.Maze is an Agent", AgentLaws (Maze).laws)
   checkAll ("concrete.simplemaze.Maze is Episodic", EpisodicLaws (Maze).laws)
