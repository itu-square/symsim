package symsim

import java.nio.file.{Paths, Files}
import java.io.PrintWriter
import java.nio.charset.StandardCharsets
import Option.option2Iterable

import symsim.concrete.ConcreteQTable

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
        Files.write(Paths.get(s"$p.policy"), 
          stringPolicy.getBytes(StandardCharsets.UTF_8))
      if outputQTable then 
        Files.write(Paths.get(s"$p.q"), 
          stringQ.getBytes(StandardCharsets.UTF_8))

    policies

  def evalAndLog(
    setup: concrete.ConcreteExactRL[State, ObservableState, Action],
    policies: List[setup.Policy]
  ): Unit =
    val sample_policies =
      for
        i <- 0 to 5
      yield policies (i)
    for
      i <- 0 to 5
      policy <- sample_policies
      pEval = setup.policyEval (policy)
      strR: String = pEval.mkString ("\n")
      outputR: String = s"evaluation $i.csv"
      writerR = new PrintWriter (outputR)
    do
      try {
        writerR.println (strR)
      } finally {
        writerR.close ()
      }
