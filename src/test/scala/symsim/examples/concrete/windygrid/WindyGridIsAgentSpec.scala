package symsim
package examples.concrete.windygrid
import examples.concrete.windygrid.GridAction._

class WindyGridIsAgentSpec extends SymSimSpec {

  checkAll( "concrete.windygrid.WindyGrid is an Agent",

    new laws.discipline.AgentTests[
      GridState,
      GridFiniteState,
      GridAction,
      GridReward,
      concrete.Randomized
    ].agent (WindyGrid)

  )

}
