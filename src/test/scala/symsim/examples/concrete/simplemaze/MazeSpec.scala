package symsim.examples.concrete.simplemaze

import org.scalacheck.Gen
import org.scalacheck.Prop.forAll

import symsim.CanTestIn.given
import symsim.concrete.Randomized.given

// To eliminate the warning on Simple maze, until scalacheck makes it open
import scala.language.adhocExtensions

class MazeSpec
  extends org.scalacheck.Properties ("SimpleMaze"):

  // Generators of test data

  val xs = Gen.oneOf[Int] (1, 2, 3, 4)
  val ys = Gen.oneOf[Int] (1, 2, 3)
  val actions = Gen.oneOf (Maze.instances.enumAction.membersAscending)
  val states = Gen.oneOf (Maze.instances.enumState.membersAscending)

  // Tests

  property ("Agent near to left wall, cannot go left") =
    forAll (ys) { y =>
      for (s1, r) <- Maze.step (1, y) (Left)
      yield s1._1 == 1
    }

  property ("Agent near to right wall, cannot go right") =
    forAll (ys) { y =>
      for (s1, r) <- Maze.step (4, y) (Right)
      yield s1._1 == 4
    }

  property ("Agent near to up wall, cannot go up") =
    forAll (xs) { x =>
      for (s1, r) <- Maze.step (x, 3) (Up)
      yield s1._2 == 3
    }

  property ("Agent near to down wall, cannot go down") =
    forAll (xs) { x =>
      for (s1, r) <- Maze.step (x, 1) (Down)
      yield s1._2 == 1
    }

  property ("Agent cannot go into block") =
    forAll (states, actions) { (s, a) =>
      for (s1, r) <- Maze.step (s) (a)
      yield s1 != (2, 2)
    }
