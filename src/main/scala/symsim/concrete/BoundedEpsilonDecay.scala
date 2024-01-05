package symsim
package concrete

trait BoundedEpsilonDecay: 

  def decayFactor: Double = 0.99
  def minExploration: Double = 0.00001

  def decay (ε: Probability): Probability = 
    if ε <= minExploration then ε else ε * decayFactor 
