package symsim
package concrete

import symsim.examples.concrete.mountaincar.MountainCar

import MountainCar.instances.given

/** This test is just a sanity check - it mostly tests Bdl against
 *  itself (so an equality check).
 */
class BdlConcreteExpectedSarsaIsExpectedSarsaSpec
  extends SymSimSpec:

  val csarsa = BdlConcreteExpectedSarsa (
    agent = MountainCar,
    alpha = 0.1,
    gamma = 0.2,
    epsilon0 = 0.0, // The update distribution test requires low ε for stability
    episodes = -1, // Not used in this test
  )

  checkAll ("concrete.BdlConcreteExpectedSarsa is Sarsa", 
    symsim.laws.SarsaLaws (csarsa).laws)
  checkAll ("concrete.BdlConcreteExpectedSarsa is ConcreteExpectedSarsa",
    symsim.laws.ConcreteExpectedSarsaLaws (csarsa, csarsa.gamma).laws)
