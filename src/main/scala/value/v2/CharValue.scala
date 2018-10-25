package value.v2

import entity.Entity

import scala.language.implicitConversions

private[value] abstract class CharValue extends Value[Char]

object CharValue {
    implicit def char2V(value: Char): CharValue = CharConstant(value)
    
    implicit class Char2V(value: Char) {
        def toValue: CharValue = value
        
        def toCharValue: CharValue = value
        
        def toStringValue: StringValue = value.toString
    }
    
    final case class CharConstant(value: Char) extends CharValue {
        override def getValue(implicit entityMap: Map[Int, Entity]): Option[Char] = Some(value)
    }
    
}
