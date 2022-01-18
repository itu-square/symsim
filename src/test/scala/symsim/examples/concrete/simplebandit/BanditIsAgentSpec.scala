package symsim
package examples.concrete.simplebandit

import laws.AgentLaws
import laws.EpisodicLaws

class BanditIsAgentSpec
   extends SymSimSpec:
   checkAll ("concrete.simplebandit.Bandit is an Agent", AgentLaws (BanditObj).laws)
   checkAll ("concrete.simplebandit.Bandit is Episodic", EpisodicLaws (BanditObj).laws)
