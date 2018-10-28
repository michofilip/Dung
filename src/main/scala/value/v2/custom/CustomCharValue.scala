package value.v2.custom

import entity.Entity
import entity.Entity.ValueHolder
import value.v2.CharValue

object CustomCharValue {
    
    final case class GetChar(id: Int) extends CharValue {
        override def getValue(implicit entityMap: Map[Int, Entity]): Option[Char] = {
            entityMap.get(id) match {
                case Some(en: ValueHolder[_]) => en.value match {
                    case v: CharValue => v.getValue
                    case _ => None
                }
                case _ => None
            }
        }
    }
    
}
