package symsim.examples.concrete.windygrid

import org.scalacheck.Gen
import org.scalacheck.Prop.forAll
import org.scalacheck.Prop.*

import symsim.concrete.Randomized.given
import symsim.concrete.ConcreteSarsa
import symsim.CanTestIn.given

// To eliminate the warning on WindyGird, until scalacheck makes it open
import scala.language.adhocExtensions

class WindyGridSpec
  extends org.scalacheck.Properties ("WindyGrid"):

  import WindyGrid.instances.{arbitraryAction, arbitraryState}

  property ("Up and Down will never affect the x value") =
    forAll { (s: GridState) => 
      for
        (s1, r) <- WindyGrid.step (s) (GridAction.U)
        (s2, r) <- WindyGrid.step (s) (GridAction.D)
      yield s1._1 == s.x && s2._1 == s.x
    }

  property ("If wind, R affects both x and y unless in the y's upper bound") =
    forAll { (s: GridState) => 
      for (s1, r) <- WindyGrid.step (s) (GridAction.R)
      yield (s.x >= 4 && s.x <= 8 && s.y <= 6) ==> (s1._1 != s.x && s1._2 != s.y)
    }

  property ("If wind, L affects both x and y unless in the y's upper bound") =
    forAll { (s: GridState) =>
      for (s1, r) <- WindyGrid.step (s) (GridAction.L)
      yield (s.x >= 4 && s.x <= 8 && s.y <= 6) ==> (s1._1 != s.x && s1._2 != s.y)
    }

  // The test runs 1 episode regardless the last argument value (`episodes`)
  val sarsa = ConcreteSarsa (WindyGrid, 0.1, 0.5, 0.1, 1)

  property ("Q-table values are non-positive") =
    forAll { (s: GridState, a: GridAction) =>
      for Q <- sarsa.learningEpisode (sarsa.initialize, s)
      yield Q (s, a) <= 0
    }
