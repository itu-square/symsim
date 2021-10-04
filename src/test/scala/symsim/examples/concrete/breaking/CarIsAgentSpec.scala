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

  checkAll( "concrete.breaking.Car is Episodic",

    new laws.discipline.EpisodicTests[
      CarState,
      CarFiniteState,
      CarAction,
      CarReward,
      concrete.Randomized
    ].agent (Car)
  )
