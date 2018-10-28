package value.v2.custom

import entity.Entity
import entity.Entity.ValueHolder
import value.v2.BooleanValue

object CustomBooleanValue {
    
    final case class GetBoolean(id: Int) extends BooleanValue {
        override def getValue(implicit entityMap: Map[Int, Entity]): Option[Boolean] = {
            entityMap.get(id) match {
                case Some(en: ValueHolder[_]) => en.value match {
                    case v: BooleanValue => v.getValue
                    case _ => None
                }
                case _ => None
            }
        }
    }
    
    final case class ExistEntity(id: Int) extends BooleanValue {
        override def getValue(implicit entityMap: Map[Int, Entity]): Option[Boolean] = {
            Some(entityMap.contains(id))
        }
    }
    
}
