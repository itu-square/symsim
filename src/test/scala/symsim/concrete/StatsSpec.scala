package symsim
package concrete

/** Sanity tests for Stats */
class StatsSpec 
  extends org.scalacheck.Properties ("concrete.Stats"):

  // The oracle values here are empirically established (I am soo
  // lazy). They roughly agree with the plot at 
  // https://stats.stackexchange.com/questions/160020/credible-set-for-beta-distribution/160035#160035
  property ("A specific test case for betaHdi") = 
    val (l, r) = betaHdi (11, 4, 0.95)
    (l - .5).abs <= 0.01 && (r - 0.91).abs <= 0.01

