package value.v2.custom

import entity.Entity
import entity.Entity.{MultiState, ValueHolder}
import states.State
import value.v2.Value

import scala.language.implicitConversions

private[value] abstract class StateValue extends Value[State]

object StateValue {
    def apply(value: State): StateConstant = StateConstant(value)
    
    implicit def st2V(value: State): StateValue = StateConstant(value)
    
    implicit class St2V(value: State) {
        def toValue: StateValue = value
        
        def toStateValue: StateValue = value
    }
    
    final case class StateConstant(value: State) extends StateValue {
        override def getValue(implicit entityMap: Map[Int, Entity]): Option[State] = Some(value)
    }
    
    final case class GetState(id: Int) extends StateValue {
        override def getValue(implicit entityMap: Map[Int, Entity]): Option[State] = {
            entityMap.get(id) match {
                case Some(en: ValueHolder[_]) => en.value match {
                    case v: StateValue => v.getValue
                    case _ => None
                }
                case Some(en: MultiState[_]) => Some(en.state)
                case _ => None
            }
        }
    }
    
}