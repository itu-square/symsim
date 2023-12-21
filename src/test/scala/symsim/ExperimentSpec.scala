package symsim

import java.nio.file.{Paths, Files}
import java.io.PrintWriter
import java.nio.charset.StandardCharsets

import symsim.concrete.ConcreteQTable

trait ExperimentSpec[State, ObservableState, Action]
  extends org.scalatest.freespec.AnyFreeSpec,
  org.scalatest.matchers.should.Matchers:

  def learnAndLog (
    setup: concrete.ConcreteExactRL[State, ObservableState, Action],
    outputQTable: Boolean = true,
    outputPolicy: Boolean = true, 
    outputToFile: Option[String] = None
  ): setup.Policy =

    val (q, r, qL) = setup.runQ
    val csvStrR: String = r.mkString("\n")
    val outputFileR: String = "accumulative reward.csv"
    val writerR = new PrintWriter(outputFileR)
    try {
      writerR.println(csvStrR)
    } finally {
      writerR.close()
    }
    val strQ: String = qL.mkString("\n")
    val outputFileQ: String = "List of Q.txt"
    val writerQ = new PrintWriter(outputFileQ)
    try {
      writerQ.println(strQ)
    } finally {
      writerQ.close()
    }
    val policy = setup.qToPolicy (q)
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
        Files.write(Paths.get(s"$p.policy"), 
          stringPolicy.getBytes(StandardCharsets.UTF_8))
      if outputQTable then 
        Files.write(Paths.get(s"$p.q"), 
          stringQ.getBytes(StandardCharsets.UTF_8))

    policy
