package symsim
package examples.concrete.windygrid

import laws.AgentLaws

class WindyGridIsAgentSpec
  extends SymSimSpec:

  checkAll ("concrete.windygrid.WindyGrid is an Agent", AgentLaws (WindyGrid).laws)
