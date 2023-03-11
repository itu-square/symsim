package symsim.concrete

import breeze.stats.distributions.{Rand, Beta, Gaussian}
import breeze.stats.distributions.Rand.VariableSeed.*

         
/** Update a gausian prior with a (pre-conjugate) Gaussian likelihood.
 *
 * I followed this note for the conjugate prior update:
 * https://people.eecs.berkeley.edu/~jordan/courses/260-spring10/lectures/lecture5.pdf
 * also John Kruschke, Doing Bayesian Data Analysis, 2nd Ed.
 * p. 453 (the formulation below is from there).
 * See also Christian P. Robert. The Bayesian Choice. Springer. p. 121. 
 *
 * TODO: check if this is not provided by breeze already; if so
 * remove
 *
 * @return the posterior mean and variance
 */
def GaussianPosterior (prior_μ: Double, prior_σ2: Double, likelihood_σ2: Double)
  (observations: Seq[Double]): (Double, Double) = 

  val n = observations.size
  val observationMean = observations.sum / n.toDouble

  // Posterior params for sample of size `n` with mean `mean`
  def μ_post (n: Int, mean: Double) = 
    val nd = n.toDouble
    (1.0/prior_σ2 / (nd/likelihood_σ2 + 1.0/prior_σ2)) * prior_μ + 
      (nd/likelihood_σ2 / (nd/likelihood_σ2 + 1.0/prior_σ2)) * mean 

  // τ is 1/σ
  def τ_post (n: Int) =
    val nd = n.toDouble
    1.0/prior_σ2 + nd/likelihood_σ2

  val post_μ = μ_post (n, observationMean)
  val post_σ2 = 1.0 / τ_post (n)

  (post_μ, post_σ2)


/** Numerically finds a minimum of the function f, using x0 as a seed
 *  (starting point).
 *
 *  For the time being the gradient is approximated numerically,
 *  although for our current use, we could probably find an analytical
 *  solution. This would likely make the tests faster and less flaky.
 */
def minimize (f: Double => Double, x0: Double): Double =
  import breeze.optimize.*
  import breeze.linalg.DenseVector
  import breeze.stats.distributions.*
  
  // I am wrapping it into vector of size 1, because I do not
  // understand how to force breeze to optimize a function directly on
  // a Double value.  This likely needs to be fixed one day
  val differrentiableF = 
    ApproximateGradientFunction { (x: DenseVector[Double]) => f(x(0)) }

  val lbfgs = new LBFGS[DenseVector[Double]] (maxIter = 100, m = 4) 
  lbfgs.minimize(differrentiableF, DenseVector(x0)) (0)

/** Compute an highest density interval of mass p for density Beta(α, β).
 *
 *  This is approximate (because of a numerical optimization step)
 *  Apparently, the procedure is quite general and we could use it for
 *  other distributions too, but for the time being we are trying it
 *  for Beta.
 *  
 *  We only handle nondegenerate cases, with α and β bigger than one.
 *
 *  Source: whuber (https://stats.stackexchange.com/users/919/whuber),
 *  Credible set for beta distribution, URL (version: 2015-07-05):
 *  https://stats.stackexchange.com/q/160035
 *
 *  Also: Andrey Akinshin. Trimmed Harrell-Davis quantile estimator
 *  based on the highest density interval of the given width.  (but we
 *  do given probability mass not given width).
 *
 *
 *  @param α the number of successes observed (+1), σ > 1
 *  @param β the number of failures observed (+1), β > 1
 *  @param p the probability mass of the sought credibility interval
 *
 *  @return the endpoints of the HDI of mass `p` for density β(α, β).
 */
def betaHdi (α: Double, β: Double, p: Double): (Double, Double) = 

  require (α > 1.0)
  require (β > 1.0)
  require (p >= 0.0)
  require (p <= 1.0)

  val pc = 1.0 - p
  val distr = Beta (α, β)
  val mode = distr.mode
  val l0 = distr.inverseCdf (pc / 2.0)

  /** The objective function for the search.  We need to find a zero
   *  in 2D space of this function.
   */
  def f (l: Double): Double = 
    val pl = if l <= 0 then 0.0
             else if l < 1.0 then distr.cdf (l)
             else 1.0
    val r = distr.inverseCdf ((pl+p) min 1.0)
    val pr = if r <= 0 then 0.0
             else if r < 1.0 then distr.cdf (r)
             else 1.0
    val diff = pl - pr 
    val penalty = pr - pl - p // penalty if the interval is too small
    diff * diff + penalty * penalty

  assert { 0 <= l0 && l0 <= 1.0 }
  val l = minimize (f, l0)
  (l, distr.inverseCdf (distr.cdf (l) + p))
