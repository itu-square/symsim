package symsim
package concrete

import symsim.concrete.ConcreteSarsa
import cats.Monad

class ConcreteSarsaSpec
  extends org.scalatest.freespec.AnyFreeSpec
  with org.scalatest.matchers.should.Matchers:

   // Import evidence that states and actions can be enumerated
   import UnitAgent._

   val sarsa = ConcreteSarsa[
      UnitState,
      UnitState,
      UnitAction
   ] (
      agent = UnitAgent,
      alpha = 0.1,
      gamma = 0.1,
      epsilon = 0.2, // explore vs exploit ratio
      episodes = 150000,
   )

   val C = 555555

   "This should stack overflow (checking the stack size)" in {
      def h (n: Int): Int = if n == 0 then 1 else h(n-1) + 1
      assertThrows[java.lang.StackOverflowError] { h (C) }
   }

   "This shouldn't overflow stack (ItereateUntilM with learn1 is tailrec, each episode tailrec)"  in {
      var c = C
      val p = { (qs: (sarsa.Q,UnitState)) => c = c - 1; c == 0  }
      val initial = sarsa.initQ -> ()
      val f = (sarsa.learn1 _).tupled
      summon[Monad[Randomized]]
        .iterateUntilM[(sarsa.Q,UnitState)] (initial) (f) (p)
        .map { _._1 }
        .head
   }


   "initQ terminates (regression)"  in {
      sarsa.initQ
   }

   "learn is tail recursive (regression)"  in {
      sarsa.learn (sarsa.initQ).head
      // this test does not really prove the tail recursiveness
   }

   "runQ is tail recursive (regression)"  in {
      sarsa.learnN (sarsa.episodes, sarsa.initQ).head
   }
