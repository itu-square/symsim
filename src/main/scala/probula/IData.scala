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

/** A tiny pure Bayesian inference framework based on rejection sampling.
  * A small alternative for the core part of Figaro.
  */
package probula

type SampleSize = Int
type Chain[T] = LazyList[T]

/** Representation of Inference Data. The intention is to make it
 *  compatible with the inference data in the Python world (oen day...)
 */
case class IData[+T](name: Name, chain: Chain[T]): 

  /** The sameple size */
  lazy val size: Int = this.chain.size

  private[probula] def map[S](f: T => S): IData[S] = 
    IData(this.name, this.chain.map(f))

  def flatMap[S](f: T => IData[S]): IData[S] =
    val newChain: Chain[S] = 
      this.chain.map[S] { t => f(t).chain.head }
    this.copy (chain = newChain)

  private[probula] def map2[S,U](that: IData[S])(f: (T,S) => U): IData[U] = 
    val newChain = this.chain.zip(that.chain).map(f.tupled)
    IData(Name.No, newChain)

  private[probula] def filter(p: T => Boolean): IData[T] = 
    this.copy(chain = this.chain.filter(p))

  def label(name: Name): IData[T] = 
    this.copy (name = name)

  def label(name: String): IData[T] = 
    label(name.toName)

  /** Show the data as a string, for the prefix of at most 100 elements 
   */
  override def toString : String = 
    val elipsis = if this.chain.drop(100).isEmpty then "" else "..."
    s"""${this.name}: ${this.chain.take(100).mkString(",")} ${elipsis}"""

  /** Estimate the probability of an event defined by the predicate `p`.
    * Precondition - the sample size is finite. 
    **/
  def probability[S >: T](p: S => Boolean): Double = 
    chain.filter(p).length.toDouble / this.size.toDouble

  /** Estimate the probability of an event defined by `value`.
    * Precondition - the sample size is finite. 
    **/
  def probability[S >: T](value: S): Double = 
    this.probability { _ == value }

  /** Same as `probability`. */
  def pr[S >: T](p: S => Boolean): Double =
    this.probability(p)

  /** Same as `probability`. */
  def pr[S >: T](value: S): Double = 
    this.probability(value)

  /** What is the probability of the sample part that matches the
   *  given case pattern?
   */
  def prMatching[A] (f: PartialFunction[T, A]): Double =
    this.pr(f.isDefinedAt)

import math.Numeric.Implicits.infixNumericOps
extension [T](ego: IData[T])

  /** Compute a median of a sampole with a defined `Ordering`.
   *
   *  For simplicity we drop one element if the sample is of even
   *  length. Typically to be used on a univariate sample of numbers
   *  (then the ordering exists).
   */
  def median(using Ordering[T]): T =
    val N = ego.size
    val M = N - (N % 2)
    val s = (if (N != M) ego.chain.tail else ego.chain).sorted
    s(M / 2) 

  /** Compute a mean for a numeric sample. 
   *  Note: This fails for large samples due to overflow 
   */
  def mean(using math.Numeric[T]): Double =
    ego.chain.sum.toDouble / ego.size

  /** Same as `mean` */
  def expectedValue(using math.Numeric[T]): Double = mean
