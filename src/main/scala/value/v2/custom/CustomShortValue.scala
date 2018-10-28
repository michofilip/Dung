package value.v2.custom

import entity.Entity
import entity.Entity.ValueHolder
import value.v2.ShortValue

object CustomShortValue {
    
    final case class GetShort(id: Int) extends ShortValue {
        override def getValue(implicit entityMap: Map[Int, Entity]): Option[Short] = {
            entityMap.get(id) match {
                case Some(en: ValueHolder[_]) => en.value match {
                    case v: ShortValue => v.getValue
                    case _ => None
                }
                case _ => None
            }
        }
    }
    
}
