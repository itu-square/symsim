package symsim
package examples.concrete.windygrid

import laws.AgentLaws

class WindyGridIsAgentSpec
  extends SymSimSpec:

  implicit override val generatorDrivenConfig =
    PropertyCheckConfiguration(minSuccessful = 100)

  checkAll ("concrete.windygrid.WindyGrid is an Agent", AgentLaws (WindyGrid).laws)
