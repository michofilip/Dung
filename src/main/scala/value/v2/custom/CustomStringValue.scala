package value.v2.custom

import entity.Entity
import entity.Entity.ValueHolder
import value.v2.StringValue

object CustomStringValue {
    
    final case class GetString(id: Int) extends StringValue {
        override def getValue(implicit entityMap: Map[Int, Entity]): Option[String] = {
            entityMap.get(id) match {
                case Some(en: ValueHolder[_]) => en.value match {
                    case v: StringValue => v.getValue
                    case _ => None
                }
                case _ => None
            }
        }
    }
    
}
