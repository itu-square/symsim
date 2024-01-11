/** A tiny pure Bayesian inference framework based on rejection sampling.
  * A small alternative for the core part of Figaro.
  *
  * Copyright (c) 2023 Andrzej Wasowski
  *
  * Licensed under the Apache License, Version 2.0 (the "License");
  * you may not use this file except in compliance with the License.
  * You may obtain a copy of the License at
  *
  *   http://www.apache.org/licenses/LICENSE-2.0
  *
  * Unless required by applicable law or agreed to in writing, software
  * distributed under the License is distributed on an "AS IS" BASIS,
  * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  * See the License for the specific language governing permissions and
  * limitations under the License.
*/
package probula
package test

import org.scalacheck.Prop.forAll
import org.scalacheck.Prop

given spire.random.rng.SecureJava = 
  spire.random.rng.SecureJava.apply


object ProbulaSpec
  extends org.scalacheck.Properties("probula"):

  /** A fake forAll, to rerun tests multiple times with various seeds. */
  def many (prop: => Prop): Prop = 
    forAll { (n: Int) => prop }

  val N = 10
  val M = 500

  val x = Uniform(0, 1, 2, 3, 4, 5)
  val y = Uniform(20, 21, 22) 

  property("010 _mapped variables are dependent") = many:
    val model: Dist[(Int, Int)] = x._map { _ + 1 }
    val sample: IData[(Int, Int)] = model.sample(N)
    sample.chain.forall { (x,y) => x == y - 1 }

  property("020 _map2ed variables are dependent") = many:
    val model: Dist[(Int, Int, Int)] = x._map2(y) { _ + _ }
    val sample: IData[(Int, Int, Int)] = model.sample(N)
    sample.chain.forall { (x,y,z) => x + y == z }

  property("030 mapped variables are independent") = many:
    val y = x.map { _ + 1 }
    val model: Dist[(Int, Int)] = x -> y
    val sample: IData[(Int, Int)] = model.sample(M)
    sample.chain.exists { (x,y) => x != y - 1 }

  property("040 _map2ed variables are independent") = many:
    val z: Dist[Int] = x.map2(y) { _ + _ }
    val model: Dist[((Int, Int), Int)] = x -> y -> z
    val sample: IData[((Int, Int), Int)] = model.sample(M)
    sample.chain.exists { case ((x, y), z) => x + y != z }

  property("050 x -> x are independent (if x not-deterministic)") = many:
    val model: Dist[(Int, Int)] = x -> x
    val sample: IData[(Int, Int)] = model.sample(M)
    sample.chain.exists { (x,y) => x != y }

  property("060 flatMapped variables are dependent") = many:
    def f (x: Int): Dist[Int] = 
      Uniform.apply(List(-4*x, -2*x)*)
    val model: Dist[Int] = x.flatMap(f)
    val sample: IData[Int] = model.sample(M)
    sample.chain.forall { _.abs % 2 == 0 }
      && sample.chain.forall { x => x >= -20 && x <= 0}
      && (0 to 5).forall { n => sample.chain.exists { _ == -4 * n } }
      && (0 to 5).forall { n => sample.chain.exists { _ == -2 * n } }

  property("070 Uniform(a) is Dirac") = many:
    val model = Uniform(42)
    val sample = model.sample(M)
    sample.chain.forall { _ == 42 }

  property("090 x -> x gives an independent pair") =
    val model: Dist[(Int, Int)] = x -> x
    val sample: IData[(Int, Int)] = model.sample(M)
    sample.chain.exists { (x, y) => x != y }

  property("110 conditioning removes false values") = many: 
    val model = y.condition { _ == 20 }
    val sample = model.sample(N)
    sample.chain.forall { _ == 20 }

  property("120 conditioning makes condition prob. 1") = 
    val model = y.condition { _ == 20 }
    val sample = model.sample(N)
    sample.probability { _ == 20 } == 1.0

  // Commented out tests that are not implemented to reduce confusion
  // property("130 declutter also cleans up the name structure") = 
  //   false

  // property("140 Bernoulli behaves well with p = 0 ") = false
  // property("150 Bernoulli behaves well with p = 1.0 ") = false
  // property("160 Bernoulli behaves well with p = 0.5 ") = false

/* These tests are disabled, as this will not work with a clean
 * for-yield. Requires more work.

  property("170 withFilter behaves like filter (1 variable)") = 
    val model = for x <- Bernoulli() if x yield x
    model.sample(N).chain.forall(identity[Boolean])

  property("171 withFilter behaves like filter (2 variables)") = 
    val model = for 
      x <- Bernoulli(.5)
      y <- Bernoulli(.5)
      if x
    yield (x,y)
    val model1 =
      Bernoulli(.5).flatMap { x => 
        Bernoulli(.5).withFilter { y => x }.map { y => 
    (x,y) } }
    println(model.sample(N))
    false

  property("180 map after withFilter behaves like map") = false
*/

  // property("190 sample(N).size = N") = false
  // property("200 conditioning twice has the same effect as once") = false
  // property("210 mean of constant sample") = false
  // property("220 mean of other deterministic sample") = false
  // property("230 median of constant sample") = false
  // property("240 median is >= half of the elements in a sample") = false
  // property("250 median is <= half of the eleemnts in a sample") = false
