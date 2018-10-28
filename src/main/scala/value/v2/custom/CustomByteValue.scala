package value.v2.custom

import entity.Entity
import entity.Entity.ValueHolder
import value.v2.ByteValue

object CustomByteValue {
    
    final case class GetByte(id: Int) extends ByteValue {
        override def getValue(implicit entityMap: Map[Int, Entity]): Option[Byte] = {
            entityMap.get(id) match {
                case Some(en: ValueHolder[_]) => en.value match {
                    case v: ByteValue => v.getValue
                    case _ => None
                }
                case _ => None
            }
        }
    }
    
}
