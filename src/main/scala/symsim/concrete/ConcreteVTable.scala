package symsim
package concrete

import cats.syntax.all.*

trait ConcreteVTable[State, ObservableState, Action]
  extends VTable[State, ObservableState, Action, Double, Randomized2]:

  this: ConcreteExactRL[State, ObservableState, Action] =>

  import agent.instances.*

  type Scheduler[A] = Randomized2[A]

  /** TODO: is this function used by VTable algorithms at all? 
    * I have replaced it by unimplemented for now, for safety. */
  def bestAction (v: V) (s: State): Action = ???
    
  def chooseAction (v: V) (s: State): Scheduler[Action] = for
    explore <- Randomized2.coin (this.epsilon0)
    action <- if explore
              then Randomized2.oneOf (allActions*)
              else Randomized2.const (bestAction (v) (s))
  yield action
