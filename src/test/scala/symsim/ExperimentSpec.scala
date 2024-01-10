package symsim

import java.nio.file.{Paths, Files}
import java.io.PrintWriter
import java.nio.charset.StandardCharsets

import symsim.concrete.ConcreteQTable
import symsim.concrete.Randomized
import symsim.concrete.Randomized.sample

trait ExperimentSpec[State, ObservableState, Action]
  extends org.scalatest.freespec.AnyFreeSpec,
  org.scalatest.matchers.should.Matchers:

  def learnAndLog (
    setup: concrete.ConcreteExactRL[State, ObservableState, Action],
    outputQTable: Boolean = true,
    outputPolicy: Boolean = true, 
    outputToFile: Option[String] = None
  ): List[setup.Policy] =

    val (q, qL) = setup.runQ
    val policy = setup.qToPolicy (q)
    val policies = qL.map (setup.qToPolicy)
    val policyOutput = setup.pp_policy (policy)
    val qOutput = setup.vf.pp_Q (q)

    outputToFile match

      case None => 
        val stringPolicy = policyOutput.hang (4).render (80)
        val stringQ = qOutput.hang (4).render (80)
        if outputPolicy then info (stringPolicy)
        if outputQTable then info (stringQ)

      case Some (p) =>
        val stringPolicy = policyOutput.render (800)
        val stringQ = qOutput.render (800)
        if outputPolicy then 
          Files.write (Paths.get (s"$p.policy"), 
            stringPolicy.getBytes (StandardCharsets.UTF_8))
        if outputQTable then 
          Files.write (Paths.get (s"$p.q"), 
            stringQ.getBytes (StandardCharsets.UTF_8))

    policies

  opaque type EvaluationResults = List[List[Double]]


  /** Evaluate a list of policies.
   *  *  
   *  @param filePath The name of the file were the results are to be stored,
   *                  including path and extension.
   *  @param initials An optional generator of initial states (if None
   *                  setup.agent.initialize is used)
   *  @param noOfEpisodes The number of episodes to be run. For each evaluation
   *                  episode we first choose an initial state from (@link initials) 
   *                  and then run an episode. Thus each of the samples might 
   *                  be initiated in a different initial state if initials 
   *                  is not a Dirac distribution.
   */
  def eval (
    setup: concrete.ConcreteExactRL [State, ObservableState, Action],
    policies: List[setup.Policy], 
    initials: Option[Randomized[State]] = None,
    noOfEpisodes: Int = 5
  ):  EvaluationResults = 
    val ss: Randomized[State] = initials.getOrElse (setup.agent.initialize)
    for p <- policies
        episodeRewards: Randomized[Randomized[Double]] = setup.evaluate (p, ss)
        rewards: Randomized[Double] = episodeRewards.map { e => e.sample () }
    yield rewards.take (noOfEpisodes).toList  


  def mean(l: List[Double]): Double = 
    l.sum / l.length.toDouble

  def meanStd (l: List[Double]): (Double, Double) = 
    val μ  = mean (l)
    val σσ = mean (l.map (x => (x - μ)*(x -μ)))
    (μ, scala.math.sqrt (σσ))


  /** Extension methods for saving experiment results */
  extension (results: EvaluationResults) 

    /** Produce a single CSV file with index, mean, and variance per policy.
     */
    def save (filePath: String): Unit = 
      val μσ: List[(Double, Double)]= results.map (meanStd)
      val indexed: List[((Double, Double), Int)] = μσ.zipWithIndex
      val output = indexed.map { case ((μ, σ), i) => s"${i}, ${μ}, ${σ}\n" }
                          .mkString
      val writerR = new PrintWriter (filePath)
      try writerR.println (output)
      finally writerR.close ()

  /** Produce a CSV file for each policy storing Reward and result for all
   *  episodes for each policy
   */
    def saveVerbose (filePath: String): Unit = 
      for (r, i) <- results.zipWithIndex do 
        val output   = r.mkString ("\n")
        val fileName =  filePath + s".$i"
        val writerR  = new PrintWriter (fileName)
        try writerR.println (output)
        finally writerR.close ()
