package symsim.examples.concrete.cliffWalking

import org.scalacheck.Gen
import org.scalacheck.Prop.forAll

import symsim.CanTestIn.given
import symsim.concrete.Randomized.given

import CliffWalking.instances.{arbitraryState, arbitraryAction}

// Eliminate the warning on CliffWalkingSpec, until scalacheck makes it open
import scala.language.adhocExtensions

object CliffWalkingSpec
  extends org.scalacheck.Properties ("cliffwalking.CliffWalking"):

    property("All rewards are negative") =
      forAll { (s: CWState, a: CWAction) =>
        for (_, r) <- CliffWalking.step (s) (a)
        yield r < 0
      }
