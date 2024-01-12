package symsim.examples.concrete.cliffWalking

import cats.syntax.all.* 
import org.scalacheck.Gen
import org.scalacheck.Prop.forAll

import symsim.CanTestIn.given
import symsim.concrete.Randomized.given

private val cliffWalking = 
  new CliffWalking (using spire.random.rng.SecureJava.apply)
import cliffWalking.instances.{arbitraryState, arbitraryAction, canTestInScheduler}

// Eliminate the warning on CliffWalkingSpec, until scalacheck makes it open
import scala.language.adhocExtensions

object CliffWalkingSpec
  extends org.scalacheck.Properties ("cliffwalking.CliffWalking"):

    property("All rewards are negative") =
      forAll { (s: CWState, a: CWAction) =>
        for (_, r) <- cliffWalking.step (s) (a)
        yield r < 0
      }
