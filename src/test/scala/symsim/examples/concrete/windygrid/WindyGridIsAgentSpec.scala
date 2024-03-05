package symsim
package examples.concrete.windygrid

import laws.AgentLaws

private val windyGrid = new WindyGrid (using spire.random.rng.SecureJava.apply)

class WindyGridIsAgentSpec
  extends SymSimSpec:

  checkAll ("concrete.windygrid.WindyGrid is an Agent", AgentLaws (windyGrid).laws)
