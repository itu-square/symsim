package symsim
package concrete.bdl

enum Est: 
  case Sample (gamma: Double)
  case Expectation (gamma: Double)

  def γ: Double = this match 
  case Sample (gamma) => gamma
  case Expectation (gamma) => gamma

import Est.*

/** A BDL term corresponding to an entire back-up diagram 
 *
 *  @param est a sequence of predictive estimation steps (these are executed)
 *  @param alpha the learning rate parameter of a RL update 
 *  @param update the final update --- the last step in the diagram
 */
case class Update (est: List[Est], alpha: Double, update: Est):

  def α: Double = this.alpha

