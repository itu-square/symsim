package symsim

import java.nio.file.{Paths, Files}
import java.nio.charset.StandardCharsets

import symsim.concrete.ConcreteQTable

trait ExperimentSpec[State, ObservableState, Action]
  extends org.scalatest.freespec.AnyFreeSpec,
  org.scalatest.matchers.should.Matchers:

   def learnAndLog (setup: concrete.ConcreteExactRL[State, ObservableState, Action]
     with ConcreteQTable[State, ObservableState, Action],
     outputQTable: Boolean = true,
     outputPolicy: Boolean = true, 
     outputToFile: Option[String] = None) =

     val q = setup.runQ
     val policy = setup.qToPolicy (q)
     val policyOutput = setup.pp_policy (policy)
     val qOutput = setup.pp_Q (q)


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