package symsim
package laws

import breeze.stats.distributions.{Rand, Beta}
import breeze.stats.distributions.Rand.VariableSeed.*
import scala.language.postfixOps

import org.scalacheck.Prop.*
import org.scalacheck.Arbitrary

import symsim.concrete.ConcreteExactRL
import symsim.concrete.ConcreteQTable
import symsim.concrete.Randomized

/** Laws that have to be obeyed by any refinement of symsim.ConcreetSarsa
 *
 * @param sarsa A   problem  configured   together  with   a  learning
 *              algorithm. The  problem has  to be  rather stable,  so
 *              preferably ε  = 0.  Otherwise, the  noise may diffuse
 *              the difference between the udpate distributions in the
 *              BDL test.
 *
 * @param gamma The discount  factor. The gamma  argument needs  to be
 *              provided separately, because  ConcreteExactRL might be
 *              an  algorithm  that  does  not  have  a  single  gamma
 *              (this  interface is  quite abstract). We  presently do
 *              not  have an  interface for  algorithms with  a single
 *              discount factor, so we  have to use ConcreteExactRL or
 *              Sarsa/ConcreteSarsa. The latter is too concrete, as it
 *              would disallow running the  testsuite on the BDL-based
 *              implementations,  which is  a convenient  sanity check
 *              (the test of bdl against bdl should pass).
 */
case class ConcreteSarsaLaws[State, ObservableState, Action]
  (sarsa: ConcreteExactRL[State, ObservableState, Action],
   gamma: Double
  ) extends org.typelevel.discipline.Laws:

  import sarsa.{agent,vf}
  import sarsa.agent.instances.given
  import sarsa.vf.{Q, apply, updated}

  val γ = gamma

  // A shortcut for instantiating the interpreter with the right term for SARSA
  val bdl =  
    symsim.concrete.BdlConcreteSarsa[State, ObservableState, Action] 
      (agent, sarsa.α, this.γ, sarsa.ε, -1)

  given Arbitrary[Q] = 
    Arbitrary (vf.genVF (using agent.instances.arbitraryReward))
  
  val laws: RuleSet = SimpleRuleSet (
    "concreteSarsa",

    "probability of not choosing the best action is smaller than ε" ->
      forAllNoShrink { (q: Q, a_t: Action) =>

        val n = 4000
        val ε = 0.1 // Ignore ε in the problem as it might be zero for
                    // the sake of the other test
        
        val trials = for 
          s_t  <- agent.initialize
          a_tt <- vf.chooseAction (ε) (q) (agent.observe (s_t))
        yield a_tt != vf.bestAction (q) (agent.observe (s_t))

        // We implement this as a Bayesian test, checking whether htere is
        // 0.95 belief that the probability of suboptimal action is ≤ ε.
        // We check this by computing the posterior and asking CDF (ε) ≥ 0.94

        val successes = trials.take (n).count { _ == true }
        val failures = n - successes

        // α=1 and β=1 gives a prior, weak flat, unbiased
        val cdfEpsilon =  Beta (2 + successes, 2 + failures).cdf (ε)

        (cdfEpsilon >= 0.94) :| 
          s"""|The beta posterior test results (failing):
              |    posterior_cdf(${ε}) == $cdfEpsilon
              |    #exploration selections ≠ best action: $successes 
              |    #best action selections: $failures 
              |    #total trials: $n""".stripMargin
    },

    // TODO: this test is alsmost the same as for ConcreteExpectedSarsaLaws.
    // It only differs in the spec. There is an opportunity for
    // parameterizing with the spec, and possibly also for extracting
    // the distribution difference test to a separate function.
    "The update distribution produced by an update follows Eq. 15 (BDL)" ->
       forAllNoShrink { (q_t: Q, s_t: State, a_t: Action) =>
         
         // #samples for the distribution test
         val n = 2000 

         val os_t = agent.observe (s_t)

         // call the tested implementation
         val sut: Randomized[(Q, List[Double], State, Action)] =
           Randomized.repeat (sarsa.learningEpoch (q_t, List[Double](), s_t, a_t))

         // call the spec interpreter
         val spec: Randomized[(Q, List[Double], State, Action)] =
           Randomized.repeat (bdl.learningEpoch (q_t, List[Double](), s_t, a_t))

         // We find a Bayesian posterior (analytically) for the mean
         // difference between them means, and checking whether we
         // believe the difference is close to zero with small
         // standard deviation.
         
         // Extract a univariate distributions over updates
         
         val sutUpdates = sut.map { (q_tt, _, _, _) => q_tt (os_t, a_t) }
         val specUpdates = spec.map { (q_tt, _, _, _) => q_tt (os_t, a_t) }

         // A random variable representing differences between updates

         val diffs = (sutUpdates zip specUpdates)
           .map { _ - _ }
           .take (n)
          
         // Infer the posterior on mean update.
         // Prior: we assume zero reward, with large standard deviation
         // It would be nice to know something about the range of possible rewards
         // We do have some rewards in the range of 1e+4.
         val μ_0 = 0.0
         val σ2_0 = .5e+4 

         // Likelihood (μ distributed with the prior, σ fixed for now).
         // Isn't it fair to fix a small σ2 because the estimation of
         // mean should be very stable? The literature is a bit quiet
         // on how to select σ2 for this estimator of posterior μ.
         // OTOH choosing low σ2 makes the likelihood function hard to
         // overcome with experiments?
         val σ2 = 10

         val (μ_post_diff, σ2_post_diff) = 
           symsim.concrete.GaussianPosterior (μ_0, σ2_0, σ2) (diffs)

         // Is the expected difference of the updates close to zero?
         val tolerance = 0.025
         val ci_mass = symsim.concrete.GaussianMass (μ_post_diff, σ2_post_diff) 
           (-tolerance, +tolerance)

         val msg = s"""|The normal posterior test results (failing):
                       |    ci mass == ${ci_mass}
                       |    "μ_diff: $μ_post_diff, σ2_diff: $σ2_post_diff""".stripMargin

         (ci_mass >= 0.95) :| msg
       },

       "The value of variables in the Q table are not NaN after learning (divergence)" -> 
       forAllNoShrink { (q_t: Q, s_t: State) =>
          val initials = Randomized.repeat (agent.initialize).take (10)
          val schedule = sarsa.learn (q_t, List[List[Double]](), initials)
          val q = schedule.head._1
          val os_t = agent.observe (s_t)
          vf.actionValues (q) (os_t)
            .values
            .forall { x => !x.isNaN && !x.isInfinity}
       },
  )
