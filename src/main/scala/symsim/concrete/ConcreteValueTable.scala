package symsim
package concrete

trait ConcreteVTable[State, ObservableState, Action]
  extends VTable[State, ObservableState, Action, Double, Randomized]:

  this: ConcreteExactRL[State, ObservableState, Action] =>

  import agent.instances.*

  type Scheduler[A] = Randomized[A]

  def bestAction (v: V) (s: State): Action =
    val vaa = for
      a <- allActions
      sa_tt <- agent.step (s) (a)
      (s_tt, _) = sa_tt
    yield (v (agent.discretize (s_tt)), a)
    vaa.sortWith { _._1 >= _._1 }.head._2

  def chooseAction (v: V) (s: State): Scheduler[Action] =
    for
      explore <- Randomized.coin (this.epsilon)
      action <- if explore
      then Randomized.oneOf (allActions: _*)
      else Randomized.const (bestAction (v) (s))
    yield action
