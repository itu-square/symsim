package symsim
package examples.concrete.breaking

class CarIsAgentSpec extends SymSimSpec:

  checkAll( "concrete.breaking.Car is an Agent",

    new laws.discipline.AgentTests[
      CarState,
      CarFiniteState,
      CarAction,
      CarReward,
      concrete.Randomized
    ].agent (Car)

  )
