package value.v2.custom

import entity.Entity
import entity.Entity.{Directional, ValueHolder}
import states.Direction
import value.v2.Value

import scala.language.implicitConversions

private[value] abstract class DirectionValue extends Value[Direction]

object DirectionValue {
    def apply(value: Direction): DirectionConstant = DirectionConstant(value)
    
    implicit def dir2V(value: Direction): DirectionValue = DirectionConstant(value)
    
    implicit class Dir2V(value: Direction) {
        def toValue: DirectionValue = value
        
        def toStateValue: DirectionValue = value
    }
    
    final case class DirectionConstant(value: Direction) extends DirectionValue {
        override def getValue(implicit entityMap: Map[Int, Entity]): Option[Direction] = Some(value)
    }
    
    final case class GetDirection(id: Int) extends DirectionValue {
        override def getValue(implicit entityMap: Map[Int, Entity]): Option[Direction] = {
            entityMap.get(id) match {
                case Some(en: ValueHolder[_]) => en.value match {
                    case v: DirectionValue => v.getValue
                    case _ => None
                }
                case Some(en: Directional[_]) => Some(en.direction)
                case _ => None
            }
        }
    }
    
}