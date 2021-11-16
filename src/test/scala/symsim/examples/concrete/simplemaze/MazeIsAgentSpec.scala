package symsim
package examples.concrete.simplemaze

import laws.AgentLaws

class MazeIsAgentSpec
   extends SymSimSpec:
   checkAll ("concrete.simplemaze.Maze is an Agent", AgentLaws (Maze).laws)
