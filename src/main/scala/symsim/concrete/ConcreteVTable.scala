package symsim
package concrete

trait ConcreteVTable[State, ObservableState, Action]
  extends VTable[State, ObservableState, Action, Double, Randomized]:

  this: ConcreteExactRL[State, ObservableState, Action] =>

  import agent.instances.*

  type Scheduler[A] = Randomized[A]

  /** TODO: is this function used by VTable algorithms at all? 
    * I have replaced it by unimplemented for now, for safety. */
  def bestAction (v: V) (s: State): Action = ???
    
  def chooseAction (v: V) (s: State): Scheduler[Action] = for
    explore <- Randomized.coin (this.epsilon0)
    action <- if explore
    then Randomized.oneOf (allActions*)
    else Randomized.const (bestAction (v) (s))
  yield action
