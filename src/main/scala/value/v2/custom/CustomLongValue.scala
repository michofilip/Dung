package value.v2.custom

import entity.Entity
import entity.Entity.{MultiState, TimeCounter, ValueHolder}
import value.v2.LongValue

object CustomLongValue {
    
    final case class GetLong(id: Int) extends LongValue {
        override def getValue(implicit entityMap: Map[Int, Entity]): Option[Long] = {
            entityMap.get(id) match {
                case Some(en: ValueHolder[_]) => en.value match {
                    case v: LongValue => v.getValue
                    case _ => None
                }
                case _ => None
            }
        }
    }
    
    final case class GetMapTime(id: Int) extends LongValue {
        override def getValue(implicit entityMap: Map[Int, Entity]): Option[Long] = {
            entityMap.get(id) match {
                case Some(en: TimeCounter[_]) => Some(en.time)
                case _ => None
            }
        }
    }
    
    final case class GetStateChangeTime(id: Int) extends LongValue {
        override def getValue(implicit entityMap: Map[Int, Entity]): Option[Long] = {
            entityMap.get(id) match {
                case Some(en: MultiState[_]) => Some(en.stateChangeTime)
                case _ => None
            }
        }
    }
    
}
