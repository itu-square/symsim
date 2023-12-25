package symsim

import java.nio.file.{Paths, Files}
import java.io.PrintWriter
import java.nio.charset.StandardCharsets

import symsim.concrete.ConcreteQTable
import symsim.concrete.Randomized
//import symsim.concrete.Randomized.{mean, variance}

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




  /** Evaluate a list of policies and produce a single CSV file with index,
   *  mean, and variance. 
   *  
   *  @param filePath The name of the file were the results are to be stored,
   *  including path and extension.
   */

  def evalAndLog(
    setup: concrete.ConcreteExactRL [State, ObservableState, Action],
    policies: List[setup.Policy], 
    filePath: String 
  ): Unit = 
    val rewardSamples = policies.map { setup.evaluate }
    val means = rewardSamples.map { r => r.sum/r.length}
    val variances = rewardSamples.map { r =>
      {
        val μ  = r.sum/r.length
        val μl = r.map (x => (x - μ) * (x - μ))
        μl.sum/μl.length
      }
    }
    val μσσ: List[((Double, Double), Int)] = (means zip variances).zipWithIndex
    val output        = μσσ.map { case ((μ, σσ), i) => s"${i}, ${μ}, ${σσ}\n" }
                           .mkString
    val writerR       = new PrintWriter (filePath)
    try writerR.println (output)
    finally writerR.close ()



  /** Evaluate a list of policies and produce a single CSV for each policy
   *  storing Reward and result for all episodes for each policy
   *  
   *  @param filePath The name of the file were the results are to be stored,
   *  including path and extension. The file names created will have an
   *  additional extension with the policy index.
   */
  def evalAndLogVerbose(
    setup: concrete.ConcreteExactRL [State, ObservableState, Action],
    policies: List[setup.Policy], 
    filePath: String 
  ): Unit = 

    for (p, i) <- policies.zipWithIndex do 
      val rewards: List[Double] = setup.evaluate (p).toList
      val output   = rewards.mkString ("\n")
      val fileName =  filePath + ".$i"
      val writerR  = new PrintWriter (fileName)
      try writerR.println (output)
      finally writerR.close ()
