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

enum Name: 
  case Simple(name: String)
  case Tuple(names: Name*)
  case Suffixed(stem: Name, suffix: String)
  case No

  override def toString: String = this match
    case Simple(name) => 
      name

    case Tuple(names*) => 
      names
        .map { _.toString }
        .mkString("[", ",", "]")

    case Suffixed(stem, suffix) =>
      s"$stem.$suffix"

    case No => "■"

  infix def -> (that: Name) = 
    Name.Tuple (this, that)

  infix def :: (other: Name) = other match 
    case Name.Tuple(names*) => 
      Name.Tuple((names.prepended (this))*)
    case _ => this -> other

extension (s: String) 
  def toName: Name = Name.Simple(s)
extension (names: (Name, Name))
  def toName: Name = Name.Tuple(names.toList*)
extension (names: (Name, Name, Name))
  def toName: Name = Name.Tuple(names.toList*)


