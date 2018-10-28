package value.v2.custom

import entity.Entity
import entity.Entity.ValueHolder
import value.v2.DoubleValue

object CustomDoubleValue {
    
    final case class GetDouble(id: Int) extends DoubleValue {
        override def getValue(implicit entityMap: Map[Int, Entity]): Option[Double] = {
            entityMap.get(id) match {
                case Some(en: ValueHolder[_]) => en.value match {
                    case v: DoubleValue => v.getValue
                    case _ => None
                }
                case _ => None
            }
        }
    }
    
}
