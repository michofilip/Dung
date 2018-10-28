package value.v2.custom

import entity.Entity
import entity.Entity.ValueHolder
import value.v2.FloatValue

object CustomFloatValue {
    
    final case class GetFloat(id: Int) extends FloatValue {
        override def getValue(implicit entityMap: Map[Int, Entity]): Option[Float] = {
            entityMap.get(id) match {
                case Some(en: ValueHolder[_]) => en.value match {
                    case v: FloatValue => v.getValue
                    case _ => None
                }
                case _ => None
            }
        }
    }
    
}
