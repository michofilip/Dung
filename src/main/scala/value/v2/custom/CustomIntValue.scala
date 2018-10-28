package value.v2.custom

import entity.Entity
import entity.Entity.{MapEntity, ValueHolder}
import value.v2.IntValue

object CustomIntValue {
    
    final case class GetInt(id: Int) extends IntValue {
        override def getValue(implicit entityMap: Map[Int, Entity]): Option[Int] = {
            entityMap.get(id) match {
                case Some(en: ValueHolder[_]) => en.value match {
                    case v: IntValue => v.getValue
                    case _ => None
                }
                case _ => None
            }
        }
    }
    
    final case class GetX(id: Int) extends IntValue {
        override def getValue(implicit entityMap: Map[Int, Entity]): Option[Int] = {
            entityMap.get(id) match {
                case Some(en: MapEntity[_]) => Some(en.position.x)
                case _ => None
            }
        }
    }
    
    final case class GetY(id: Int) extends IntValue {
        override def getValue(implicit entityMap: Map[Int, Entity]): Option[Int] = {
            entityMap.get(id) match {
                case Some(en: MapEntity[_]) => Some(en.position.y)
                case _ => None
            }
        }
    }
    
}
