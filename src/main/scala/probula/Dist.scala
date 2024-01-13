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

/** A tiny pure Bayesian inference framework based on rejection
 *  sampling. A small alternative for the core part of Figaro.
 */
package probula

import scala.annotation.targetName

import spire.random.Uniform.*
import spire.random.Dist.*
import spire.random.*

type RNG = spire.random.Generator

/** A representation of probabilistic models as multivariate
 *  distributions,  effectively hierarchical Bayesian models.
 */
trait Dist[+T]: 
  self =>

  // Abstract members

  /** The name of the random variable, represented using a qualified
   *  name term.  For a multivariate model, the name is typically
   *  interpreted as the name of the last variable added (the last in
   *  the tuple).  This is not ideal and will have to be redesigned in
   *  the future.
   *
   *  @see [[probula.Name]]
   */
  def name: Name

  /** Generate a sample from `this` model.  The sample is an infinite
   *  lazy list (underneath).  Typically this method is not used by 
   *  users of the library. They will use the variant that takes a
   *  sample size.
   */
  def sample[U >: T](using RNG): IData[U]

  // Derived members

  /** Generate a sample from `this` model containing `n` samples.
   */
  def sample[U >: T](n: SampleSize)(using RNG): IData[U] = 
    val idata = this.sample(using summon[RNG])
    idata.copy(chain = idata.chain.take(n))

  /** Create a univariate[1] model by mapping a function on the `this`
   *  model. Users should typically use the `detDep` function
   *  instead, as `map` "hides" all the existing variables in the
   *  model, and the resulting model is univariate, regardless how
   *  many variables `this` had.
   *
   *  [1] Technically the resulting model does not have to be
   *  univariate, even though it typically is. It will have the arity
   *  of the type `U`.
   *
   * @param name The descriptive name of the only variable in the new
   *             model.
   * @see [[detDep]]
   */
  def map[U](l: Name)(f: T => U): Dist[U] = new Dist[U]:
    val name = l
    def sample[V >: U](using rng: RNG): IData[V] = 
      self.sample.map(f).label(l)

  /** Create a univariate[1] model by mapping a function on the `this`
   *  model. Users should typically use the `detDep` function
   *  instead, as `map` "hides" all the existing variables in the
   *  model, and the resulting model is univariate, regardless how
   *  many variables `this` had.
   *
   *  [1] Technically the resulting model does not have to be
   *  univariate, even though it typically is. It will have the arity
   *  of type `U`.
   *
   * @param name The descriptive name of the only variable in the new
   *             model.
   * @see [[detDep]]
   */
  def map[U](l: String)(f: T => U): Dist[U] =
    self.map(l.toName)(f)

  /** Create a univariate[1] model by mapping a function on the `this`
   *  model. Users should typically use the `detDep` function
   *  instead, as `map` "hides" all the existing variables in the
   *  model, and the resulting model is univariate, regardless how
   *  many variables `this` had.
   *
   *  The variable in the model resulting model will have the same
   *  descriptive name as the `this` node had, but suffixed with
   *  `map`.
   *
   *  [1] Technically the resulting model does not have to be
   *  univariate, even though it typically is. It will have the arity
   *  of type `U`.
   *
   * @see [[detDep]]
   */
  def map[U](f: T => U): Dist[U] =
    val l = Name.Suffixed(self.name, "map")
    self.map(l)(f)

  /** Add a new variable (dimension) to `this` model by mapping
   *  function `f`.  The new variable is just added to the current
   *  model, so the arity of the model increases by one. Users should
   *  use [[detDep]] instead.
   *  
   * @param name The descriptive name of the newly added variable 
   * @see [[detDep]]
   */
  def _map[U](l: Name)(f: T => U): Dist[(T,U)] = 
    self.map { t => (t, f(t)) }
      .label(self.name -> l)

  /** Add a new variable (dimension) to `this` model by mapping
   *  function `f`.  The new variable is just added to the current
   *  model, so the arity of the model increases by one. Users should
   *  use [[detDep]] instead.
   *  
   * @param name The descriptive name of the newly added variable 
   * @see [[detDep]]
   */
  def _map[U](l: String)(f: T => U): Dist[(T,U)] = 
    self._map(l.toName)(f)

  /** Add a new variable (dimension) to `this` model by mapping
   *  function `f`.  The new variable is just added to the current
   *  model, so the arity of the model increases by one. Users should
   *  use [[detDep]] instead.
   *
   *  The new variable will be using the same name as `this` but
   *  suffixed with `_map`.
   *  
   * @see [[detDep]]
   */
  def _map[U](f: T => U): Dist[(T,U)] = 
    val newName = this.name -> Name.Suffixed(name, "_map")
    self._map(newName)(f)


  def map2[U, V](other: Dist[U]) (f: (T,U) => V): Dist[V] =  new Dist[V]:
    val name: Name = Name.Suffixed (self.name -> other.name, "map2")
    def sample[W >: V](using rng: RNG): IData[W] = 
      self.sample.map2(other.sample)(f)
       .label(this.name)

  def map2[U, V](other: Dist[U]) (f: ((T,U)) => V): Dist[V] = 
    self.map2(other)(scala.Function.untupled(f))

  def _map2[U, V](other: Dist[U]) (f: (T,U) => V): Dist[(T,U,V)] = 
    self.map2(other)((t,u) => (t, u, f(t,u)))
  def _map2[U, V](that: Dist[U]) (f: ((T,U)) => V): Dist[(T,U,V)] = 
    _map2(that)(scala.Function.untupled(f))

  /** Zip two variables into one bivariate variables. This should
   *  typically be used only for variables constructed with basic
   *  constructors (independent). 
   */
  def zip[U](that: Dist[U]): Dist[(T,U)] = 
    this.map2(that)(identity[(T,U)])

  /** Same as `zip`. */
  infix def -> [U](that: Dist[U]): Dist[(T,U)] = 
    this.zip(that)

  /** Create a univariate[1] model by mapping function `f` the `this`
   *  model. 
   *
   *  Users should typically use the `probDep` function
   *  instead, as `flatMap` "hides" all the existing variables in the
   *  model, and the resulting model is univariate, regardless how
   *  many variables `this` had.
   *
   *  [1] Technically the resulting model does not have to be
   *  univariate, even though it typically is. It will have the arity
   *  of type `U`.
   *
   * @param l    The descriptive name of the only variable in the new
   *             model.
   * @param f    The function to be mapped.
   * @return     The model created by mapping. All original variables
   *             are hidden (lost)
   * @see [[probDep]]
   */
  def flatMap[U](l: Name)(f: T => Dist[U]): Dist[U] = new Dist:
    def name: Name = l 
    def sample[S >: U](using RNG): IData[S] =
      self.sample.flatMap(t => f(t).sample(1))

  /** Create a univariate[1] model by mapping function `f` the `this`
   *  model. 
   *
   *  Users should typically use the `probDep` function
   *  instead, as `flatMap` "hides" all the existing variables in the
   *  model, and the resulting model is univariate, regardless how
   *  many variables `this` had.
   *
   *  [1] Technically the resulting model does not have to be
   *  univariate, even though it typically is. It will have the arity
   *  of type `U`.
   *
   * @param l    The descriptive name of the only variable in the new
   *             model.
   * @param f    The function to be mapped.
   * @return     The model created by mapping. All original variables
   *             are hidden (lost)
   * @see [[probDep]]
   */
  def flatMap[U](l: String)(f: T => Dist[U]): Dist[U] =
    self.flatMap(l.toName)(f)


  /** Create a univariate[1] model by mapping function `f` on `this`
   *  model. 
   *
   *  Users should typically use the `probDep` function
   *  instead, as `flatMap` "hides" all the existing variables in the
   *  model, and the resulting model is univariate, regardless how
   *  many variables `this` had.
   *
   *  [1] Technically the resulting model does not have to be
   *  univariate, even though it typically is. It will have the arity
   *  of type `U`.
   *
   * @param f    The function to be mapped.
   * @return     The model created by mapping. All original variables
   *             are hidden (lost). The new model carries the same
   *             name as `this` but suffixed with `flatMap`.
   * @see [[probDep]]
   */
  def flatMap[U](f: T => Dist[U]): Dist[U] =
    self.flatMap(Name.Suffixed(self.name, "flatMap"))(f)

  /** Add a new variable to the `this` model  by flatMapping function
   *  `f`. 
   *
   *  Users should typically use the `probDep` function
   *  instead, as `_flatMap`. The resulting model has arity higher by
   *  1 than `this`.
   *
   * @param l    The descriptive name of the only variable in the new
   *             model.
   * @param f    The function to be flatMapped to derive the new
   *             variable.
   * @return     The model created by flatMapping and adding the
   *             result to the existing model tuple. 
   * @see [[probDep]]
   */
  def _flatMap[U](l: Name)(f: T => Dist[U]): Dist[(T,U)] = 
    self.flatMap { t => f(t).map { u => (t, u) } }

  /** Add a new variable to the `this` model  by flatMapping function
   *  `f`. 
   *
   *  Users should typically use the `probDep` function
   *  instead, as `_flatMap`. The resulting model has arity higher by
   *  1 than `this`.
   *
   * @param l    The descriptive name of the only variable in the new
   *             model.
   * @param f    The function to be flatMapped to derive the new
   *             variable.
   * @return     The model created by flatMapping and adding the
   *             result to the existing model tuple. 
   * @see [[probDep]]
   */
  def _flatMap[U](l: String)(f: T => Dist[U]): Dist[(T,U)] = 
    self._flatMap(l.toString)(f)

  /** Add a new variable to the `this` model  by flatMapping function
   *  `f`. 
   *
   *  Users should typically use the `probDep` function
   *  instead, as `_flatMap`. The resulting model has arity higher by
   *  1 than `this`. The new variable is given no name.
   *
   * @param f    The function to be flatMapped to derive the new
   *             variable.
   * @return     The model created by flatMapping and adding the
   *             result to the existing model tuple. 
   * @see [[probDep]]
   */
  def _flatMap[U](f: T => Dist[U]): Dist[(T,U)] = 
    self._flatMap(Name.No)(f)

  /** Change the name of the current model/variable to an arbitrary
   *  string `l`.
   */
  def label(l: String): Dist[T] = 
    self.label(l.toName)

  /** Change the name of the current model/variable to an arbitrary
   *  new name `l`.
   */
  def label(l: Name): Dist[T] = new Dist[T]:
    override def name: Name = l
    override def sample[S >: T](using rng: RNG): IData[S] = 
      self.sample.label(name)

  /** Reject all the values of the model that do not satisfy `p`.
   *
   *  This is also called observe in literature (and since we only do
   *  rejection sampling for now it is equivalent to conditioning).
   *  The new filtered model will have the same name as `this` but
   *  suffixed with `filter`.
   */
  def filter(p: T => Boolean): Dist[T] = new Dist[T]: 
    def name = Name.Suffixed(self.name, "filter")
    override def sample[S >: T](using rng: RNG): IData[S] =
      self.sample.filter(p)

  /** Reject all the values of the model that differ from `value`
   *  usign a usual equality test `==`.
   *
   *  This is also called observe in literature (and since we only do
   *  rejection sampling for now it is equivalent to conditioning).
   *  The new filtered model will have the same name as `this` but
   *  suffixed with `filter`.
   */
  def filter[S >: T](value: S): Dist[T] = 
    self.filter { _ == value }

  // Disabled, as the semantics is unexpected
  // def withFilter(p: T => Boolean): WithFilter = new WithFilter(p)

  /** Not used presently. */
  class WithFilter(p: T => Boolean):
    def map[U](f: T => U): Dist[U] =
      self.filter(p).map(f)
    def withFilter(q: T => Boolean): WithFilter = 
      WithFilter(x => p(x) && q(x))

  /** Reject all the values of the model that do not satisfy `p`.
   *
   *  This is also called observe in literature (and since we only do
   *  rejection sampling for now it is equivalent to conditioning).
   *  The new filtered model will have the same name as `this` but
   *  suffixed with `filter`.
   *  
   *  The same as [[filter]].
   */
  def condition(p: T => Boolean): Dist[T] = 
    self.filter(p)

  /** Reject all the values of the model that differ from `value`
   *  usign a usual equality test `==`.
   *
   *  This is also called observe in literature (and since we only do
   *  rejection sampling for now it is equivalent to conditioning).
   *  The new filtered model will have the same name as `this` but
   *  suffixed with `filter`.
   *
   *  The same as [[filter]].
   */
  def condition[S >: T](value: S): Dist[T] = 
    self.filter(value)

  /** Reject all the values of the model that do not match the pattern
   *  in the partial fucntion `f` (so more precisely for which `f` is
   *  not defined).  In practice `f` is a _case+ claose without a body
   *  as the body is not executed by `matching`.
   *
   *  This is also called observe in literature (and since we only do
   *  rejection sampling for now it is equivalent to conditioning).
   *  The new filtered model will have the same name as `this` but
   *  suffixed with `filter`.
   */
  def matching[A](f: PartialFunction[T, A]): Dist[T] =
    self.filter(f.isDefinedAt)


// NB. Overloaded extension methods cannot compete with
// trait methods (apparentely then the trait methods are
// tried only) - for these reason even Dist[T] must use extensions,
// so that we can support other arities.
extension [T](self: Dist[T]) 

  /** Define a new variable with name `l` that deterministically
   *  depends on the values of existing variables in the model. 
   *  Does the same thing as `_map` with name.
   */
  def detDep[U](l: Name)(f: T => U): Dist[(T, U)] = 
    self._map(l)(f)

  /** Define a new variable (without a name) that deterministically
   *  depends on the values of existing variables in the model. 
   *  Does the same thing as `_map`
   */
  def detDep[U](f: T => U): Dist[(T, U)] = 
    self._map(f)

  /** Define a new variable with name `l` that deterministically
   *  depends on the values of existing variables in the model. 
   *  Does the same thing as `_map` with name.
   */
  def detDep[U](l: String)(f: T => U): Dist[(T,U)] = 
    self._map(l)(f)

  /** Define a new variable with name `l` that probabilistically
   *  depends on the values of existing variables in the model. 
   *  Does the same thing as `_flatMap` with name.
   */
  def probDep[U](l: Name)(f: T => Dist[U]): Dist[(T, U)] =
    self._flatMap(l)(f)

  /** Define a new variable with name `l` that probabilistically
   *  depends on the values of existing variables in the model. 
   *  Does the same thing as `_flatMap` with name.
   */
  def probDep[U](l: String)(f: T => Dist[U]): Dist[(T, U)] =
    self.probDep(l.toName)(f)

  /** Define a new variable with a generated name that probabilistically
   *  depends on the values of existing variables in the model. 
   *  Does the same thing as `_flatMap` (The generated name is different).
   */
  def probDep[U](f: T => Dist[U]): Dist[(T, U)] =
    self.probDep(Name.Suffixed(self.name, "probDep"))(f)

  /** Add a new uniformly distributed variable to a model. */
  def uniform[U](name: Name)(values: U*): Dist[(T,U)] = for 
    t <- self
    u <- Uniform[U](name)(values*)
  yield (t, u)

  /** Add a new uniformly distributed variable to a model. */
  def uniform[U](name: String)(values: U*): Dist[(T,U)] = 
    self.uniform(name.toName)(values*)

  /** Add a new uniformly distributed variable to a model. */
  def uniform[U](values: U*): Dist[(T,U)] = 
    self.uniform(Name.No)(values*)

  /** Add a new Bernoulli variable to a model. */
  def bernoulli[U](name: Name)(p: Double, success: U, failure: U): Dist[(T, U)] = for
    t <- self
    u <- Bernoulli(name, p, success, failure)
  yield (t, u)

  /** Add a new Bernoulli variable to a model. */
  def bernoulli[U](name: String)(p: Double, success: U, failure: U): Dist[(T, U)] =
    self.bernoulli(name.toName)(p, success, failure)

  /** Add a new Bernoulli variable to a model. */
  def bernoulli[U](p: Double, success: U, failure: U): Dist[(T, U)] =
    self.bernoulli(Name.No)(p, success, failure)

  /** Add a new Bernoulli variable to a model. */
  def bernoulli[U](name: Name)(f: T => Double, success: U, failure: U)
    : Dist[(T, U)] = for
      t <- self
      p  = f(t)
      u <- Bernoulli(name, p, success, failure)
    yield (t, u)

  /** Add a new Bernoulli variable to a model. */
  def bernoulli[U](name: String)(f: T => Double, success: U, failure: U)
    : Dist[(T, U)] = 
    self.bernoulli(name.toName)(f, success, failure)

  /** Add a new Bernoulli variable to a model. */
  def bernoulli[U](f: T => Double, success: U, failure: U)
    : Dist[(T, U)] = 
    self.bernoulli(Name.No)(f, success, failure)


// This "tupled causes some problems so I would like to remove it"
extension [S, T](self: Dist[(S, T)])

  /** Define a new variable with name `l` that deterministically
   *  depends on the values of existing variables in the model. 
   *  Does the same thing as `_map` with name.
   */
  @targetName("detDep2name")
  def detDep[U](l: Name)(f: (S, T) => U)
    : Dist[(S, T, U)] =
    self._map(l)(f.tupled).declutter

  /** Define a new variable with name `l` that deterministically
   *  depends on the values of existing variables in the model. 
   *  Does the same thing as `_map` with name.
   */
  @targetName("detDep2string")
  def detDep[U](l: String)(f: (S, T) => U): Dist[(S, T, U)] =
    self.detDep[U](l.toName)(f)

  /** Define a new variable without a name that deterministically
   *  depends on the values of existing variables in the model.
   *  Behaves consistently with `_map` without a name - it reuses the
   *  name from `this` and suffixes it with `detDep`.
   */
  @targetName("detDep2")
  def detDep[U](f: (S, T) => U): Dist[(S, T, U)] =
    self.detDep[U](Name.Suffixed(self.name, "detDep"))(f)

  /** Define a new variable with name `l` that probabilistically
   *  depends on the values of existing variables in the model. 
   *  Does the same thing as `_flatMap` with name.
   */
  @targetName("probDep2name")
  def probDep[U](l: Name)(f: (S, T) => Dist[U]): Dist[(S, T, U)] =
    self._flatMap(l)(f.tupled).declutter

  /** Define a new variable with name `l` that probabilistically
   *  depends on the values of existing variables in the model. 
   *  Does the same thing as `_flatMap` with name.
   */
  @targetName("probDep2string")
  def probDep[U](l: String)(f: (S, T) => Dist[U]): Dist[(S, T, U)] =
    self.probDep[U](l.toName)(f)

  /** Define a new variable with a generated name that probabilistically
   *  depends on the values of existing variables in the model. 
   *  Does the same thing as `_flatMap` (The generated name is different).
   */
  @targetName("probDep2")
  def probDep[U](f: (S, T) => Dist[U]): Dist[(S, T, U)] =
    self.probDep[U](Name.Suffixed(self.name, "probDep"))(f)

  /** Simplify the model to only show the first variable. */
  @targetName("firstOfTwo")
  def _1: Dist[S] = self.map { _._1 }

  /** Simplify the model to only show the second variable. */
  @targetName("secondOfTwo")
  def _2: Dist[T] = self.map { _._2 }

  /** Add a new uniform variable to a model. */
  @targetName("uniform2name")
  def uniform[U](name: Name)(values: U*): Dist[(S, T, U)] = for 
    (s,t) <- self
    u     <- Uniform[U](name)(values*)
  yield (s, t, u)

  /** Add a new uniform variable to a model. */
  @targetName("uniform2string")
  def uniform[U](name: String)(values: U*): Dist[(S, T, U)] = 
    self.uniform(name.toName)(values*)

  /** Add a new Bernoulli variable to a model. */
  @targetName("bernoulli2name")
  def bernoulli[U](name: Name)(p: Double, success: U, failure: U): Dist[(S, T, U)] = for
    (s, t) <- self
    u <- Bernoulli(name, p, success, failure)
  yield (s, t, u)

  /** Add a new Bernoulli variable to a model. */
  @targetName("bernoulli2string")
  def bernoulli[U](name: String)(p: Double, success: U, failure: U): Dist[(S, T, U)] =
    self.bernoulli(name.toName)(p, success, failure)

  /** Add a new Bernoulli variable to a model. */
  @targetName("bernoulli2")
  def bernoulli[U](p: Double, success: U, failure: U): Dist[(S, T, U)] =
    self.bernoulli(Name.No)(p, success, failure)

  /** Add a new Bernoulli variable to a model. */
  @targetName("bernoulliDep2name")
  def bernoulli[U](name: Name)(success: U, failure: U)(f: (S, T) => Double)
    : Dist[(S, T, U)] = for
      (s, t) <- self
      p  = f(s, t)
      u <- Bernoulli(name, p, success, failure)
    yield (s, t, u)

  /** Add a new Bernoulli variable to a model. */
  @targetName("bernoulliDep2string")
  def bernoulli[U](name: String)(success: U, failure: U)(f: (S, T) => Double)
    : Dist[(S, T, U)] = 
    self.bernoulli(name.toName)(success, failure)(f)

  /** Add a new Bernoulli variable to a model. */
  @targetName("bernoulliDep2")
  def bernoulli[U](success: U, failure: U)(f: (S, T) => Double)
    : Dist[(S, T, U)] = 
    self.bernoulli(Name.No)(success, failure)(f)

extension [T1, T2, T3](self: Dist[(T1, T2, T3)])

  /** Define a new variable with name `l` that deterministically
   *  depends on the values of existing variables in the model. 
   *  Does the same thing as `_map` with name.
   */
  @targetName("detDep3name")
  def detDep[T](l: Name)(f: (T1, T2, T3) => T)
    : Dist[(T1, T2, T3, T)] =
    self._map(l)(f.tupled).declutter

  /** Define a new variable with name `l` that deterministically
   *  depends on the values of existing variables in the model. 
   *  Does the same thing as `_map` with name.
   */
  @targetName("detDep3string")
  def detDep[T](l: String)(f: (T1, T2, T3) => T)
    : Dist[(T1, T2, T3, T)] =
    self.detDep(l.toName)(f)

  /** Define a new variable without a name that deterministically
   *  depends on the values of existing variables in the model.
   *  Behaves consistently with `_map` without a name - it reuses the
   *  name from `this` and suffixes it with `detDep`.
   */
  @targetName("detDep3tupled")
  def detDep[T](f: (T1, T2, T3) => T)
    : Dist[(T1, T2, T3, T)] =
    self.detDep(Name.Suffixed(self.name, "map"))(f)

  /** Simplify the model to only show the first variable. */
  @targetName("firstOfThree")
  def _1: Dist[T1] = self.map { _._1 }

  /** Simplify the model to only show the second variable. */
  @targetName("secondOfThree")
  def _2: Dist[T2] = self.map { _._2 }

  /** Simplify the model to only show the third variable. */
  @targetName("thirdOfThree")
  def _3: Dist[T3] = self.map { _._3 }



extension [T1, T2, T3, T4](ego: Dist[(T1, T2, T3, T4)])

  /** Simplify the model to only show the third variable. */
  @targetName("thirdOfFour")
  def _3: Dist[T3] = ego.map { _._3 }

  /** Simplify the model to only show the fourth variable. */
  @targetName("fourthOfFour")
  def _4: Dist[T4] = ego.map { _._4 }



extension [S, T, U](ego: Dist[((S,T),U)])
  /** Flatten the model tuple to a triple. */
  @targetName("declutter2")
  def declutter: Dist[(S, T, U)] =
    ego.map { case ((s,t),u) => (s, t, u) }

extension [T1, T2, T3, U](ego: Dist[((T1, T2, T3), U)])
  /** Flatten the model tuple to a quadruple. */
  @targetName("declutter3")
  def declutter: Dist[(T1, T2, T3, U)] =
    ego.map { case ((t1, t2, t3), u) => (t1, t2, t3, u) }

// Fixed concrete distributions (roots)

case class Dirac[T](name: Name, value: T) 
  extends Dist[T]:
  def sample[S >: T](using RNG): IData[S] = 
    IData(this.name, LazyList.continually(value))

object Dirac:
  def apply[T](value: T): Dirac[T] = 
    new Dirac[T](Name.No, value)

case class Bernoulli[T](name: Name, p: Double, success: T, failure: T)
  extends Dist[T]:

  private lazy val gen = 
    spire.random.Uniform[Double](0.0, 1.0)
      .map[T] { x => if x <= p then success else failure }

  def sample[S >: T](using rng: RNG): IData[S] = 
    val chain = gen.toLazyList(rng)
    IData(this.name, chain)

object Bernoulli: 
  def apply(p: Double = 0.5): Bernoulli[Boolean] = 
    new Bernoulli[Boolean](Name.No, p, true, false)
  def apply[T](p: Double, success: T, failure: T): Bernoulli[T] = 
    new Bernoulli[T](Name.No, p, success, failure)
  def apply[T](l: String, p: Double, success: T, failure: T): Bernoulli[T] = 
    new Bernoulli[T](l.toName, p, success, failure)

  def apply(l: String, p: Double): Bernoulli[Boolean] = 
    new Bernoulli[Boolean](l.toName, p, true, false)
  def apply(l: Name, p: Double): Bernoulli[Boolean] = 
    new Bernoulli[Boolean](l, p, true, false)


case class Uniform[+T](name: Name) (values: T*) 
  extends Dist[T]:

  val rangeMin: Int = 0
  val rangeMax: Int = values.length - 1

  private lazy val indices = rangeMin to rangeMax
  lazy val valueMap: Map[Int, T] = (indices zip values).toMap

  private lazy val gen = spire.random.Uniform(rangeMin, rangeMax)

  def sample[S >: T](using rng: RNG): IData[S] = 
    val chain = gen.toLazyList(rng)
      .map(valueMap)
    IData(this.name, chain)

object Uniform: 
  // There is certainly a much more efficient way to implement this
  // for large ranges
  def apply(name: String)(min: Int, max: Int): Uniform[Int] = 
    new Uniform[Int](name.toName)(min to max *)

  def apply(min: Int, max: Int): Uniform[Int] = 
    new Uniform[Int](Name.No)(min to max*)

  def apply[T](name: String)(values: T*): Uniform[T] = 
    new Uniform[T](name.toName)(values*)

  def apply[T](values: T*): Uniform[T] = 
    new Uniform[T](Name.No)(values*)

/** sub class of distributions over double numbers / "real numbers" */
trait DistD extends Dist[Double]:
  
  protected def gen: spire.random.Dist[Double]

  def sample[S >: Double](using rng: RNG): IData[S] = 
    val chain = gen.toLazyList(rng)
    IData(this.name, chain)



case class UniformC(name: Name, lower: Double, upper: Double) 
  extends DistD:
  assert (this.lower <= this.upper)

  protected lazy val gen = 
    spire.random.Uniform[Double](lower, upper)

object UniformC: 
  def apply(lower: Double, upper: Double): UniformC = 
    new UniformC(Name.No, lower, upper)
  def apply(l: String, lower: Double, upper: Double): UniformC = 
    new UniformC(l.toName, lower, upper)



case class Gaussian(name: Name, mean: Double = 0.0, stdDev: Double = 1.0) 
  extends DistD:

  protected lazy val gen = 
    spire.random.Gaussian(mean, stdDev)

object Gaussian: 
  def apply(mean: Double, stdDev: Double): Gaussian = 
    new Gaussian(Name.No, mean, stdDev)
  def apply: Gaussian = 
    new Gaussian(Name.No, 0.0, 1.0)
  def apply(l: String, mean: Double, stdDev: Double): Gaussian = 
    new Gaussian(l.toName, mean, stdDev)
  def apply(l: String): Gaussian = 
    new Gaussian(l.toName, 0.0, 1.0)
