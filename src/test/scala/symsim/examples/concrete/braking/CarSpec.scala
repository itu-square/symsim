package symsim
package examples.concrete.braking

import org.scalacheck.Gen
import org.scalacheck.Prop.*

import symsim.CanTestIn.given
import symsim.concrete.Randomized.given

import Car.instances.{arbitraryState, arbitraryAction}

// To eliminate the warning on CarSpec, until scalacheck makes it open
import scala.language.adhocExtensions

/** Sanity tests for symsim.concrete.braking */
object CarSpec
  extends org.scalacheck.Properties ("braking.Car"):

  property("A stopped car cannot move, however much you break") = 
    forAll { (s: CarState, a: CarAction) =>
      for (s1, r) <- Car.step (s.copy (v = 0.0)) (a)
      yield s1.p == s.p
    }

  property("The car cannot move backwards by braking") = 
    forAll { (s: CarState, a: CarAction) =>
      for (s1, r) <- Car.step (s) (a)
      yield (s.v != 0 ==> s1.p >= s.p)
    }

  property("Position never becomes negative") = 
    forAll { (s: CarState, a: CarAction) =>
      for (s1, r) <- Car.step (s) (a)
      yield s1.p >= 0.0 
    }

  property("Reward is valid 1") = 
    forAll  { (s1: CarState, s2: CarState, a: CarAction) => 
      for
        (_, r1) <- Car.step (CarState (v = s1.v, p = s1.p min s2.p)) (a)
        (_, r2) <- Car.step (CarState (v = s1.v, p = s1.p max s2.p)) (a)
      yield r1 >= r2 
    }

  property("Reward is valid 2") = 
    forAll { (s1: CarState, s2: CarState, a: CarAction) => 
      for
        (_, r1) <- Car.step (CarState (v = s1.v min s2.v, p = s1.p)) (a)
        (_, r2) <- Car.step (CarState (v = s1.v max s2.v, p = s1.p)) (a)
      yield r1 >= r2 
    }

  property("Velocity is random") =
    val states = Car.initialize.take(5).toList
    val vs = states.map(s => s.v)
    vs.combinations(2).exists(pair => pair(0) != pair(1))


