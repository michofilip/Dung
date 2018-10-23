package value

import entity.Entity
import entity.Entity._
import states.{Direction, State}
import value.Value._

import scala.language.implicitConversions

object CustomValue {
    implicit def st2v(value: State): StateValue = StateConstant(value)
    
    implicit def dir2v(value: Direction): DirectionValue = DirectionConstant(value)
    
    implicit class St2v(value: State) {
        def toValue: StateValue = value
        
        def toStateValue: StateValue = value
    }
    
    implicit class Dir2v(value: Direction) {
        def toValue: DirectionValue = value
        
        def toStateValue: DirectionValue = value
    }
    
    def apply(value: State): StateConstant = StateConstant(value)
    
    def apply(value: Direction): DirectionConstant = DirectionConstant(value)
    
    // state classes
    sealed abstract class StateValue extends CustomValue[State]
    
    final case class StateConstant(value: State) extends StateValue {
        override def getValue(implicit entityMap: Map[Int, Entity]): Option[State] = Some(value)
    }
    
    // direction classes
    sealed abstract class DirectionValue extends CustomValue[Direction]
    
    final case class DirectionConstant(value: Direction) extends DirectionValue {
        override def getValue(implicit entityMap: Map[Int, Entity]): Option[Direction] = Some(value)
    }
    
    // value extractors
    final case class GetByte(id: Int) extends CustomByteValue {
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
    
    final case class GetShort(id: Int) extends CustomShortValue {
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
    
    final case class GetInt(id: Int) extends CustomIntValue {
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
    
    final case class GetLong(id: Int) extends CustomLongValue {
        override def getValue(implicit entityMap: Map[Int, Entity]): Option[Long] = {
            entityMap.get(id) match {
                case Some(en: ValueHolder[_]) => en.value match {
                    case v: LongValue => v.getValue
                    case _ => None
                }
                case _ => None
            }
        }
    }
    
    final case class GetFloat(id: Int) extends CustomFloatValue {
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
    
    final case class GetDouble(id: Int) extends CustomDoubleValue {
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
    
    final case class GetBoolean(id: Int) extends CustomBooleanValue {
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
    
    final case class GetChar(id: Int) extends CustomCharValue {
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
    
    final case class GetString(id: Int) extends CustomStringValue {
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
    
    final case class GetX(id: Int) extends CustomIntValue {
        override def getValue(implicit entityMap: Map[Int, Entity]): Option[Int] = {
            entityMap.get(id) match {
                case Some(en: MapEntity[_]) => Some(en.position.x)
                case _ => None
            }
        }
    }
    
    final case class GetY(id: Int) extends CustomIntValue {
        override def getValue(implicit entityMap: Map[Int, Entity]): Option[Int] = {
            entityMap.get(id) match {
                case Some(en: MapEntity[_]) => Some(en.position.y)
                case _ => None
            }
        }
    }
    
    final case class GetState(id: Int) extends CustomValue[State] {
        override def getValue(implicit entityMap: Map[Int, Entity]): Option[State] = {
            entityMap.get(id) match {
                case Some(en: ValueHolder[_]) => en.value match {
                    case v: StateValue => v.getValue
                    case _ => None
                }
                case Some(en: MultiState[_]) => Some(en.state)
                case _ => None
            }
        }
    }
    
    final case class GetDirection(id: Int) extends CustomValue[Direction] {
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
    
    final case class GetMapTime(id: Int) extends CustomLongValue {
        override def getValue(implicit entityMap: Map[Int, Entity]): Option[Long] = {
            entityMap.get(id) match {
                case Some(en: TimeCounter[_]) => Some(en.time)
                case _ => None
            }
        }
    }
    
    final case class GetStateChangeTime(id: Int) extends CustomLongValue {
        override def getValue(implicit entityMap: Map[Int, Entity]): Option[Long] = {
            entityMap.get(id) match {
                case Some(en: MultiState[_]) => Some(en.stateChangeTime)
                case _ => None
            }
        }
    }
    
    final case class ExistEntity(id: Int) extends CustomBooleanValue {
        override def getValue(implicit entityMap: Map[Int, Entity]): Option[Boolean] = {
            Some(entityMap.contains(id))
        }
    }
    
}
