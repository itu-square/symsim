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

  // Generators of test data

  val xs = Gen.choose[Int] (1, 10)
  val ys = Gen.choose[Int] (1, 7)
  val actions = Gen.oneOf (WindyGrid.instances.enumAction.membersAscending)

  // Tests

  property ("Up and Down will never affect the x value") =
    forAll (xs, ys) { (x, y) =>
      for
        (s1, r) <- WindyGrid.step (GridState (x, y)) (GridAction.U)
        (s2, r) <- WindyGrid.step (GridState (x, y)) (GridAction.D)
      yield s1._1 == x && s2._1 == x
    }

  property ("When there is wind, R will affect both x and y unless in the y's upper bound") =
    forAll (xs, ys) { (x, y) =>
      for
        (s1, r) <- WindyGrid.step (GridState (x, y)) (GridAction.R)
      yield (x >= 4 && x <= 8 && y <= 6) ==> (s1._1 != x && s1._2 != y)
    }

  property ("When there is wind, L will affect both x and y unless in the y's upper bound") =
    forAll (xs, ys) { (x, y) =>
      for
        (s1, r) <- WindyGrid.step (GridState (x, y)) (GridAction.L)
      yield (x >= 4 && x <= 8 && y <= 6) ==> (s1._1 != x && s1._2 != y)
    }

  property ("Q-table values are non-positive") =
    val sarsa = ConcreteSarsa (WindyGrid, 0.1, 1, 0.05, 100000)
    forAll (xs, ys, actions) { (x, y, a) =>
      for
        Q <- sarsa.learningEpisode (sarsa.initialize, GridState (x, y))
      yield Q (GridState (x, y)) (a) <= 0
    }
