package symsim
package laws

import scala.language.postfixOps

import org.scalacheck.Prop.*
import org.scalacheck.Arbitrary

import breeze.stats.distributions.{Rand, Beta, Gaussian}
import breeze.stats.distributions.Rand.VariableSeed.*

import symsim.concrete.ConcreteSarsa
import symsim.concrete.Randomized

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
          a_tt <- chooseAction (q) (agent.discretize (s_t))
        yield a_tt != bestAction (q) (agent.discretize (s_t))

        // We implement this as a bayesian test, checking whether htere is
        // 0.95 belief that the probability of suboptimal action is ≤ ε.
        // We check this by computing the posterior and asking CDF (ε) ≥ 0.95
        // Using the number of episodes as the #trials is a bit of an abuse

        val successes = trials.take (episodes).count { _ == true }
        val failures = episodes - successes

        // α=1 and β=1 gives a prior, weak flat, unbiased
        val cdfEpsilon =  Beta (2 + successes, 2 + failures).cdf (epsilon)

        (cdfEpsilon >= 0.995) :| 
          s"""|The beta posterior test results (failing):
              |    posterior_cdf(${epsilon}) == $cdfEpsilon
              |    #exploration selections ≠ best action: $successes 
              |    #best action selections: $failures 
              |    #total trials: $episodes""".stripMargin
    },

    "The update distribution produced by an update function follows Eq. 14 (BDL)" ->
       forAllNoShrink { (q_t: Q, s_t: State, a_t: Action) =>
         
         // #samples for the distribution test
         val n = 40000 

         val os_t = agent.discretize (s_t)

         // call monolithic implementation
         val sut: Randomized[(Q, State, Action)] = 
           sarsa.learningEpoch (q_t, s_t, a_t)

         // Construct the result compositionally (inlined manually here, 
         // but in general this is an interpreter)
         //
         // Now this test looks essentially the same as learning Epoch, but if
         // we have an interpreter for BDL, it allows to test detailed
         // implementations against high-level specs. This similarity
         // is purely caused by us manually inlining the BDL semantics 
         // here for Sarsa-1 (see paper).
         val bdl = for 
           (s_tt,r_tt) <- agent.step (s_t) (a_t)
           os_tt        = agent.discretize (s_tt)
           a_tt         <- sarsa.chooseAction (q_t) (os_tt)
           g_tt         = r_tt + sarsa.gamma * q_t (os_tt) (a_tt)
           u            = q_t (os_t) (a_t) - alpha * (g_tt - q_t (os_t) (a_t))
           q_tt         = q_t + (os_tt -> (q_t (os_tt) + (a_t -> u)))
         yield (q_tt, s_tt, a_tt)

         // We do this test by assuming that both distributions are normal 
         // (A generalized test with StudentT would be even better).
         // We fir a Bayesian posterior (analytically) on the difference of
         // their means, and checking whether we believe the difference of
         // is close to zero with small standard deviation.
         //
         // I followed this note for the conjugate prior update:
         // https://people.eecs.berkeley.edu/~jordan/courses/260-spring10/lectures/lecture5.pdf
         // also John Kruschke, Doing Bayesian Data Analysis, 2nd Ed. p. 453
         // and Christian P. Robert. The Bayesian Choice. Springer. p. 121. 
         //
         // Extract a univariate distributions over updates
        
         val sutUpdates = sut.map { (q_tt, _, _) => q_tt (os_t) (a_t) }.take (n)
         val bdlUpdates = bdl.map { (q_tt, _, _) => q_tt (os_t) (a_t) }.take (n)

         val sutMean = sutUpdates.sum / n.toDouble
         val bdlMean = bdlUpdates.sum / n.toDouble

         // Prior, we assume zero reward, with large standard deviation
         // It would be nice to know something about the range of possible rewards
         // We do have some rwards in the range of 10e+4.
         val μ_0 = 0.0
         val σ2_0 = 10.0e+8 

         // Likelihood (μ distributed with the prior, σ fixed for now)
         val σ2 = 100

         // Posterior params for sample of size n with mean 'mean'
         def μ_post (n: Int, mean: Double) = 
           val nd = n.toDouble
           (1.0/σ2_0 / (nd/σ2 + 1.0/σ2_0)) * μ_0 + 
             (nd/σ2 / (nd/σ2 + 1.0/σ2_0)) * mean 

         def τ_post (n: Int) =
           val nd = n.toDouble
           1.0/σ2_0+ nd/σ2

         val μ_post_sut = μ_post (n, sutMean)
         val σ2_post_sut = 1.0 / τ_post (n)

         val μ_post_bdl = μ_post (n, bdlMean)
         val σ2_post_bdl = 1.0 / τ_post (n)

         val μ_diff = μ_post_sut - μ_post_bdl
         val σ_diff = σ2_post_sut + σ2_post_bdl

         val cdfDiff = Gaussian (μ_diff, σ_diff).cdf (0.02) 

         (cdfDiff >= 0.995) :|
          s"""|The normal posterior test results (failing):
              |    posterior_cdf(${0.01}) == $cdfDiff
              |    "μ_diff: $μ_diff, σ_diff: $σ_diff""".stripMargin
       }

  )
