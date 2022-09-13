package symsim
package concrete

class ConcreteSarsaSpec
  extends org.scalatest.freespec.AnyFreeSpec
  with org.scalatest.matchers.should.Matchers:

   // Import evidence that states and actions can be enumerated
   import UnitAgent._

   val sarsa = ConcreteSarsa (
      agent = UnitAgent,
      alpha = 0.1,
      gamma = 0.1,
      epsilon = 0.2, // explore vs exploit ratio
      episodes = 150000,
   )

   val C = 555555

   "This should stack overflow (checking the stack size C)" in {
      def h (n: Int): Int = if n == 0 then 1 else h (n-1) + 1
      assertThrows[java.lang.StackOverflowError] { h (C) }
   }

   "learnN shouldn't overflow stack (learnN is tailrec, each episode tailrec)" in {
       val initials: Randomized[UnitState] = Randomized.repeat (UnitAgent.initialize)
       val result: Randomized[sarsa.Q] = sarsa.learn (sarsa.initialize, initials.take (C))
       try result.size == 1
       catch case e =>
          fail (s"Forcing result of learning overflows (${e.toString})")
   }


   "initQ terminates, no stack overflow (regression)"  in {
      sarsa.initialize
   }

   "learn is tail recursive, no stack overflow (regression)"  in {
      val result = sarsa.learningEpisode (sarsa.initialize, ())
      result.head
      // this test does not really prove the tail recursiveness,
      // but at least checks for crash
      // also with the immediate final state 'learn' is not really tested here
   }

   "runQ is tail recursive, no stack overflow (regression)"  in {
      try sarsa.runQ
      catch case e =>
         fail (s"sarsa.runQ overflows stack (${e.toString})")
   }

end ConcreteSarsaSpec
