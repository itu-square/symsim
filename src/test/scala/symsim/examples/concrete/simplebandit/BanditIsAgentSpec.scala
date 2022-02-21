package symsim
package examples.concrete.simplebandit

import laws.AgentLaws
import laws.EpisodicLaws

class BanditIsAgentSpecConst
   extends SymSimSpec:
   checkAll ("concrete.simplebandit.Bandit is an Agent", AgentLaws (BanditObjConst).laws)
   checkAll ("concrete.simplebandit.Bandit is Episodic", EpisodicLaws (BanditObjConst).laws)
   
class BanditIsAgentSpecGaussian
   extends SymSimSpec:
   checkAll ("concrete.simplebandit.Bandit is an Agent", AgentLaws (BanditObjGaussian).laws)
   checkAll ("concrete.simplebandit.Bandit is Episodic", EpisodicLaws (BanditObjGaussian).laws)
