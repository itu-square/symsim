package symsim.examples.concrete.windygrid

import cats.syntax.all.*

import org.scalacheck.Gen
import org.scalacheck.Prop.*

import symsim.CanTestIn.given
import symsim.concrete.ConcreteSarsa
import symsim.concrete.Randomized2
import symsim.concrete.Randomized2.given

given spire.random.rng.SecureJava = spire.random.rng.SecureJava.apply

private val windyGrid = new WindyGrid 

// Eliminate the warning on WindyGridSpec until scalacheck makes Properties open
import scala.language.adhocExtensions

object WindyGridSpec
  extends org.scalacheck.Properties ("WindyGrid"):

  import windyGrid.instances.{arbitraryAction, arbitraryState, enumState, enumAction}

  property ("Up and Down will never affect the x value") =
    forAll { (s: GridState) => 
      for
        (s1, r) <- windyGrid.step (s) (GridAction.U)
        (s2, r) <- windyGrid.step (s) (GridAction.D)
      yield s1._1 == s.x && s2._1 == s.x
    }

  property ("If wind, R affects both x and y unless in the y's upper bound") =
    forAll { (s: GridState) => 
      for (s1, r) <- windyGrid.step (s) (GridAction.R)
      yield (s.x >= 4 && s.x <= 8 && s.y <= 6) ==> (s1._1 != s.x && s1._2 != s.y)
    }

  property ("If wind, L affects both x and y unless in the y's upper bound") =
    forAll { (s: GridState) =>
      for (s1, r) <- windyGrid.step (s) (GridAction.L)
      yield (s.x >= 4 && s.x <= 8 && s.y <= 6) ==> (s1._1 != s.x && s1._2 != s.y)
    }

  // The test runs 1 episode regardless the last argument value (`episodes`)
  // The fixture has to be setup here, as otherwise the parallelization seems
  // to rerun training multiple times (if it is put inside the property)
  val sarsa = ConcreteSarsa (windyGrid, 0.1, 0.5, 0.1, 1)
  val initials = LazyList (windyGrid.instances.allObservableStates*)
  val qs = sarsa.learn (sarsa.vf.initialize, List[sarsa.vf.Q] (), initials)
    .sample (5)
  import sarsa.vf.apply

  property ("Q-table values are non-positive") =
    forAll { (s: GridState, a: GridAction) => 
      qs.forall { (q, _) => q (s, a) <= 0 } }
