/* A tiny pure Bayesian inference framework based on rejection sampling.
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


/** The Probula object is used as an entry point to the framework. One
 *  starts building a probula probabilistic model by using factory
 *  methods from this object. Once the model is initiated (the first
 *  variable is created), one can use the model methods (@see
 *  probula.Dist) to add more new variables, hierarchical dependencies,
 *  and observations.
 */
object Probula:

  /** Create a model with a single uniformly distributed discrete
   *  variable named `name` (for printing).
   *
   *  @param name   A symbolic name of the variable (used for
   *                presentation)
   *  @param values A variable length argument listing enumerating the
   *                values of the uniform distribution.
   *  @return       A model of type `Dist[T]` that has a single random
   *                variable
   */
  def uniform[T](name: Name)(values: T*): Uniform[T] =
    Uniform[T](name)(values*)

  /** Create a model with a single uniformly distributed discrete
   *  random variable named `name` (for printing).
   *
   *  @param name   A symbolic name of the variable (used for
   *                presentation)
   *  @param values A variable length argument listing enumerating the
   *                values of the uniform distribution.
   *  @return       A model of type `Dist[T]` that has a single random
   *                variable
   */
  def uniform[T](name: String)(values: T*): Uniform[T] = 
    this.uniform(name.toName)(values*)

  /** Create a model with a single uniformly distributed discrete
   *  random variable without a descriptive name.
   *
   *  @param values A variable length argument listing enumerating the
   *                values of the uniform distribution.
   *  @return       A model of type `Dist[T]` that has a single random
   *                variable
   */
  def uniform[T](values: T*): Dist[T] = 
    this.uniform(Name.No)(values*): Uniform[T]

  /** Create a model with a single Bernoulli-distributed discrete
   *  random variable named `name` (for printing).
   *
   *  @param name     A symbolic name of the variable (used for
   *                  presentation)
   *  @param p        The probability of success 
   *  @param success  The value used as a success outcome (arrives with
   *                  probability `p`)
   *  @param faailure The value used as a failure outcome 
   *  @return         A model of type `Dist[T]` that has a single random
   *                  variable
   */
  def bernoulli[T](name: Name)(p: Double, success: T, failure: T): Bernoulli[T] =
    Bernoulli[T](name, p, success, failure)

  /** Create a model with a single Bernoulli-distributed discrete
   *  random variable named `name` (for printing).
   *
   *  @param name     A symbolic name of the variable (used for
   *                  presentation)
   *  @param p        The probability of success 
   *  @param success  The value used as a success outcome (arrives with
   *                  probability `p`)
   *  @param faailure The value used as a failure outcome 
   *  @return         A model of type `Dist[T]` that has a single random
   *                  variable
   */
  def bernoulli[T](name: String)(p: Double, success: T, failure: T): Bernoulli[T] =
    this.bernoulli(name.toName)(p, success, failure)

  /** Create a model with a single Bernoulli-distributed discrete
   *  random variable without a descriptive name.
   *
   *  @param p        The probability of success 
   *  @param success  The value used as a success outcome (arrives with
   *                  probability `p`)
   *  @param faailure The value used as a failure outcome 
   *  @return         A model of type `Dist[T]` that has a single random
   *                  variable
   */
  def bernoulli[T](p: Double, success: T, failure: T): Bernoulli[T] =
    this.bernoulli(Name.No)(p, success, failure)
