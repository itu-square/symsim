package symsim
package concrete

import symsim.examples.concrete.mountaincar.MountainCar

import MountainCar.instances.given

/** This test is just a sanity check - it mostly tests Bdl against
 *  itself (so an equality check).
 */
class BdlConcreteSarsaIsSarsaSpec
  extends SymSimSpec:

  val csarsa = BdlConcreteSarsa (
    agent = MountainCar,
    alpha = 0.1,
    gamma = 1.0,
    epsilon = 0.5,
    episodes = 1000,
  )

  checkAll ("concrete.ConcreteSarsa is Sarsa", 
    symsim.laws.SarsaLaws (csarsa).laws)
  checkAll ("concrete.ConcreteSarsa is ConcreteSarsa",
    symsim.laws.ConcreteSarsaLaws (csarsa, csarsa.gamma).laws)
