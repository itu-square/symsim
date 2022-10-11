package symsim
package laws

import scala.language.postfixOps

import symsim.concrete.ConcreteSarsa
import symsim.concrete.Randomized

import org.scalacheck.Arbitrary.*
import org.scalacheck.Prop
import org.scalacheck.Prop.forAll
import org.scalacheck.util.Pretty
import org.scalacheck.util.Pretty.*

import org.scalacheck.Prop.*
import org.scalacheck.Arbitrary

import breeze.stats.distributions.{Rand, Beta}

/**
 * Laws that have to be obeyed by any refinement of symsim.ConcreetSarsa
 *
 * TODO: Why is this just for Concrete? Does it have to?
 */
case class ConcreteSarsaLaws[State, FiniteState, Action]
   (sarsa: ConcreteSarsa[State, FiniteState, Action])
   extends org.typelevel.discipline.Laws:

   import sarsa.*
   import agent.instances.given

   given Arbitrary[Q] = Arbitrary (sarsa.genVF)
  
   val laws: RuleSet = SimpleRuleSet (
     "concreteSarsa",
     "probability of not choosing the best action is smaller than ε" ->
       forAllNoShrink { (q: Q, a_t: Action) =>
         
         // Two sanity checks to confirm that the generators work reasonably
         require (q.nonEmpty, 
           "The Q-Table cannot be empty") 
         require (q.values.forall { _.nonEmpty }, 
           "The entry for each state must not be empty.")

         val trials = for 
           s_t  <- agent.initialize
           a_tt <- chooseAction (q) (s_t)
         yield a_tt != bestAction (q) (s_t)

         // We implement this as a bayesian test, checking whether htere is
         // 0.95 belief that the probability of suboptimal action is ≤ ε.
         // We check this by computing the posterior and asking CDF (ε) ≥ 0.95
         // Using the number of episodes as the #trials is a bit of an abuse

         val successes = trials.take (episodes).count { _ == true }
         val failures = episodes - successes

         import Rand.VariableSeed.*
         // α=1 and β=1 gives a prior, weak flat, unbiased
         val cdfEpsilon =  Beta (2 + successes, 2 + failures).cdf (epsilon)

         (cdfEpsilon >= 0.995) :| 
           s"""|The beta posterior test results (failing):
               |    posterior_cdf(${epsilon}) == $cdfEpsilon
               |    #exploration selections ≠ best action: $successes 
               |    #best action selections: $failures 
               |    #total trials: $episodes""".stripMargin
      },
    )
