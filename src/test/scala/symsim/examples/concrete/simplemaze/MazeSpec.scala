package symsim
package examples.concrete.simplemaze

import symsim.concrete.Randomized.given
import CanTestIn.given
import org.scalacheck.{Arbitrary, Gen, Prop}
import org.scalacheck.Arbitrary.*
import org.scalacheck.Prop.forAll
import org.scalacheck.Prop.*
import examples.concrete.simplemaze.MazeState
import examples.concrete.simplemaze.Maze
import symsim.concrete.{ConcreteSarsa, Randomized}

// To eliminate the warning on Simple maze, until scalacheck makes it open
import scala.language.adhocExtensions

/** Sanity tests for Randomized as a Scheduler */
class MazeSpec
  extends org.scalacheck.Properties ("SimpleMaze"):

    // Generators of test data
    val xs = Gen.oneOf [Int](1, 2, 3, 4)
    val ys = Gen.oneOf [Int](1,2, 3)
    val actions = Gen.oneOf (Maze.instances.enumAction.membersAscending)
    val states = Gen.oneOf [(Int, Int)]((1, 1), (1, 2), (1, 3), (2, 1), (2, 3),
      (3, 1), (3, 2), (3, 3), (4, 1), (4, 2), (4, 3))


    // Tests

    property ("Agent near to left wall, cannot go left") = {
      forAll (ys) { y =>
        for (s1, r) <- Maze.step (1, y) (Left)
        yield s1._1 == 1
      }
    }

    property ("Agent near to right wall, cannot go right") = {
      forAll(ys) { y =>
        for (s1, r) <- Maze.step(4, y) (Right)
        yield s1._1 == 4
      }
    }

    property ("Agent near to up wall, cannot go up") = {
      forAll(xs) { x =>
        for (s1, r) <- Maze.step(x, 3) (Up)
        yield s1._2 == 3
      }
    }

    property ("Agent near to down wall, cannot go down") = {
      forAll (xs) { x =>
        for (s1, r) <- Maze.step(x, 1) (Down)
        yield s1._2 == 1
      }
    }

    property ("Agent cannot go into block") = {
      forAll (states, actions) { (s, a) =>
        for (s1, r) <- Maze.step(s) (a)
        yield s1 != (2, 2)
      }
    }

//    property ("checking optimal policy") = {
//      val sarsa = ConcreteSarsa(Maze, 0.1, 1, 0.05, 150000)
//      forAllNoShrink(states) { s =>
//        for
//          Q <- sarsa.learningEpisode (sarsa.initialize, s)
//        yield sarsa.bestAction (Q) (s) != Down
//      }
//    }
