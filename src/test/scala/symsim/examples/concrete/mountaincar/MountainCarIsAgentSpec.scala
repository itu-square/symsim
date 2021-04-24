package symsim
package examples.concrete.mountaincar

class MountainCarIsAgentSpec extends SymSimSpec:

  checkAll( "concrete.mountaincar.MountainCar is an Agent",

    new laws.discipline.AgentTests[
      CarState,
      CarFiniteState,
      CarAction,
      CarReward,
      concrete.Randomized
    ].agent (MountainCar)

  )
