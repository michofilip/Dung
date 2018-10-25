package value.v2

import entity.Entity
import value.v2.BooleanValue._

import scala.language.implicitConversions

private[value] abstract class BooleanValue extends Value[Boolean] {
    final def unary_! : BooleanValue = {
        this match {
            case BooleanConstant(v) => BooleanConstant(!v)
            case NOT(value) => value
            case AND(value1, value2) => NAND(value1, value2)
            case NAND(value1, value2) => AND(value1, value2)
            case OR(value1, value2) => NOR(value1, value2)
            case NOR(value1, value2) => OR(value1, value2)
            case XOR(value1, value2) => XNOR(value1, value2)
            case XNOR(value1, value2) => XOR(value1, value2)
            case Equals(value1, value2) => Unequals(value1, value2)
            case Unequals(value1, value2) => Equals(value1, value2)
            case Less(value1, value2) => GraterEqual(value1, value2)
            case LessEqual(value1, value2) => Grater(value1, value2)
            case Grater(value1, value2) => LessEqual(value1, value2)
            case GraterEqual(value1, value2) => Less(value1, value2)
            case value => NOT(value)
        }
    }
    
    final def &&(that: BooleanValue): BooleanValue = AND(this, that)
    
    final def !&&(that: BooleanValue): BooleanValue = NAND(this, that)
    
    final def ||(that: BooleanValue): BooleanValue = OR(this, that)
    
    final def !||(that: BooleanValue): BooleanValue = NOR(this, that)
    
    final def <>(that: BooleanValue): BooleanValue = XOR(this, that)
    
    final def !<>(that: BooleanValue): BooleanValue = XNOR(this, that)
}

object BooleanValue {
    implicit def bool2V(value: Boolean): BooleanValue = BooleanConstant(value)
    
    implicit class Bool2V(value: Boolean) {
        def toValue: BooleanValue = value
        
        def toBooleanValue: BooleanValue = value
        
        def toStringValue: StringValue = value.toString
    }
    
    final case class BooleanConstant(value: Boolean) extends BooleanValue {
        override def getValue(implicit entityMap: Map[Int, Entity]): Option[Boolean] = Some(value)
    }
    
    final case class NOT(value: BooleanValue) extends BooleanValue {
        override def getValue(implicit entityMap: Map[Int, Entity]): Option[Boolean] = {
            value.getValue match {
                case Some(v) => Some(!v)
                case _ => None
            }
        }
    }
    
    final case class AND(value1: BooleanValue, value2: BooleanValue) extends BooleanValue {
        override def getValue(implicit entityMap: Map[Int, Entity]): Option[Boolean] = {
            (value1.getValue, value2.getValue) match {
                case (Some(v1), Some(v2)) => Some(v1 && v2)
                case _ => None
            }
        }
    }
    
    final case class NAND(value1: BooleanValue, value2: BooleanValue) extends BooleanValue {
        override def getValue(implicit entityMap: Map[Int, Entity]): Option[Boolean] = {
            (value1.getValue, value2.getValue) match {
                case (Some(v1), Some(v2)) => Some(!(v1 && v2))
                case _ => None
            }
        }
    }
    
    final case class OR(value1: BooleanValue, value2: BooleanValue) extends BooleanValue {
        override def getValue(implicit entityMap: Map[Int, Entity]): Option[Boolean] = {
            (value1.getValue, value2.getValue) match {
                case (Some(v1), Some(v2)) => Some(v1 || v2)
                case _ => None
            }
        }
    }
    
    final case class NOR(value1: BooleanValue, value2: BooleanValue) extends BooleanValue {
        override def getValue(implicit entityMap: Map[Int, Entity]): Option[Boolean] = {
            (value1.getValue, value2.getValue) match {
                case (Some(v1), Some(v2)) => Some(!(v1 || v2))
                case _ => None
            }
        }
    }
    
    final case class XOR(value1: BooleanValue, value2: BooleanValue) extends BooleanValue {
        override def getValue(implicit entityMap: Map[Int, Entity]): Option[Boolean] = {
            (value1.getValue, value2.getValue) match {
                case (Some(v1), Some(v2)) => Some((v1 && !v2) || (!v1 && v2))
                case _ => None
            }
        }
    }
    
    final case class XNOR(value1: BooleanValue, value2: BooleanValue) extends BooleanValue {
        override def getValue(implicit entityMap: Map[Int, Entity]): Option[Boolean] = {
            (value1.getValue, value2.getValue) match {
                case (Some(v1), Some(v2)) => Some((v1 || !v2) && (!v1 || v2))
                case _ => None
            }
        }
    }
    
    final case class Equals(value1: Value[_], value2: Value[_]) extends BooleanValue {
        override def getValue(implicit entityMap: Map[Int, Entity]): Option[Boolean] = {
            (value1.getValue, value2.getValue) match {
                case (Some(v1), Some(v2)) => Some(v1 == v2)
                case _ => None
            }
        }
    }
    
    final case class Unequals(value1: Value[_], value2: Value[_]) extends BooleanValue {
        override def getValue(implicit entityMap: Map[Int, Entity]): Option[Boolean] = {
            (value1.getValue, value2.getValue) match {
                case (Some(v1), Some(v2)) => Some(v1 != v2)
                case _ => None
            }
        }
    }
    
    final case class Less(value1: OrderedValue[_], value2: OrderedValue[_]) extends BooleanValue {
        override def getValue(implicit entityMap: Map[Int, Entity]): Option[Boolean] = {
            (value1.getValue, value2.getValue) match {
                case (Some(v1: Byte), Some(v2: Byte)) => Some(v1 < v2)
                case (Some(v1: Byte), Some(v2: Short)) => Some(v1 < v2)
                case (Some(v1: Byte), Some(v2: Int)) => Some(v1 < v2)
                case (Some(v1: Byte), Some(v2: Long)) => Some(v1 < v2)
                case (Some(v1: Byte), Some(v2: Float)) => Some(v1 < v2)
                case (Some(v1: Byte), Some(v2: Double)) => Some(v1 < v2)
                
                case (Some(v1: Short), Some(v2: Byte)) => Some(v1 < v2)
                case (Some(v1: Short), Some(v2: Short)) => Some(v1 < v2)
                case (Some(v1: Short), Some(v2: Int)) => Some(v1 < v2)
                case (Some(v1: Short), Some(v2: Long)) => Some(v1 < v2)
                case (Some(v1: Short), Some(v2: Float)) => Some(v1 < v2)
                case (Some(v1: Short), Some(v2: Double)) => Some(v1 < v2)
                
                case (Some(v1: Int), Some(v2: Byte)) => Some(v1 < v2)
                case (Some(v1: Int), Some(v2: Short)) => Some(v1 < v2)
                case (Some(v1: Int), Some(v2: Int)) => Some(v1 < v2)
                case (Some(v1: Int), Some(v2: Long)) => Some(v1 < v2)
                case (Some(v1: Int), Some(v2: Float)) => Some(v1 < v2)
                case (Some(v1: Int), Some(v2: Double)) => Some(v1 < v2)
                
                case (Some(v1: Long), Some(v2: Byte)) => Some(v1 < v2)
                case (Some(v1: Long), Some(v2: Short)) => Some(v1 < v2)
                case (Some(v1: Long), Some(v2: Int)) => Some(v1 < v2)
                case (Some(v1: Long), Some(v2: Long)) => Some(v1 < v2)
                case (Some(v1: Long), Some(v2: Float)) => Some(v1 < v2)
                case (Some(v1: Long), Some(v2: Double)) => Some(v1 < v2)
                
                case (Some(v1: Float), Some(v2: Byte)) => Some(v1 < v2)
                case (Some(v1: Float), Some(v2: Short)) => Some(v1 < v2)
                case (Some(v1: Float), Some(v2: Int)) => Some(v1 < v2)
                case (Some(v1: Float), Some(v2: Long)) => Some(v1 < v2)
                case (Some(v1: Float), Some(v2: Float)) => Some(v1 < v2)
                case (Some(v1: Float), Some(v2: Double)) => Some(v1 < v2)
                
                case (Some(v1: Double), Some(v2: Byte)) => Some(v1 < v2)
                case (Some(v1: Double), Some(v2: Short)) => Some(v1 < v2)
                case (Some(v1: Double), Some(v2: Int)) => Some(v1 < v2)
                case (Some(v1: Double), Some(v2: Long)) => Some(v1 < v2)
                case (Some(v1: Double), Some(v2: Float)) => Some(v1 < v2)
                case (Some(v1: Double), Some(v2: Double)) => Some(v1 < v2)
                
                case _ => None
            }
        }
    }
    
    final case class LessEqual(value1: OrderedValue[_], value2: OrderedValue[_]) extends BooleanValue {
        override def getValue(implicit entityMap: Map[Int, Entity]): Option[Boolean] = {
            (value1.getValue, value2.getValue) match {
                case (Some(v1: Byte), Some(v2: Byte)) => Some(v1 <= v2)
                case (Some(v1: Byte), Some(v2: Short)) => Some(v1 <= v2)
                case (Some(v1: Byte), Some(v2: Int)) => Some(v1 <= v2)
                case (Some(v1: Byte), Some(v2: Long)) => Some(v1 <= v2)
                case (Some(v1: Byte), Some(v2: Float)) => Some(v1 <= v2)
                case (Some(v1: Byte), Some(v2: Double)) => Some(v1 <= v2)
                
                case (Some(v1: Short), Some(v2: Byte)) => Some(v1 <= v2)
                case (Some(v1: Short), Some(v2: Short)) => Some(v1 <= v2)
                case (Some(v1: Short), Some(v2: Int)) => Some(v1 <= v2)
                case (Some(v1: Short), Some(v2: Long)) => Some(v1 <= v2)
                case (Some(v1: Short), Some(v2: Float)) => Some(v1 <= v2)
                case (Some(v1: Short), Some(v2: Double)) => Some(v1 <= v2)
                
                case (Some(v1: Int), Some(v2: Byte)) => Some(v1 <= v2)
                case (Some(v1: Int), Some(v2: Short)) => Some(v1 <= v2)
                case (Some(v1: Int), Some(v2: Int)) => Some(v1 <= v2)
                case (Some(v1: Int), Some(v2: Long)) => Some(v1 <= v2)
                case (Some(v1: Int), Some(v2: Float)) => Some(v1 <= v2)
                case (Some(v1: Int), Some(v2: Double)) => Some(v1 <= v2)
                
                case (Some(v1: Long), Some(v2: Byte)) => Some(v1 <= v2)
                case (Some(v1: Long), Some(v2: Short)) => Some(v1 <= v2)
                case (Some(v1: Long), Some(v2: Int)) => Some(v1 <= v2)
                case (Some(v1: Long), Some(v2: Long)) => Some(v1 <= v2)
                case (Some(v1: Long), Some(v2: Float)) => Some(v1 <= v2)
                case (Some(v1: Long), Some(v2: Double)) => Some(v1 <= v2)
                
                case (Some(v1: Float), Some(v2: Byte)) => Some(v1 <= v2)
                case (Some(v1: Float), Some(v2: Short)) => Some(v1 <= v2)
                case (Some(v1: Float), Some(v2: Int)) => Some(v1 <= v2)
                case (Some(v1: Float), Some(v2: Long)) => Some(v1 <= v2)
                case (Some(v1: Float), Some(v2: Float)) => Some(v1 <= v2)
                case (Some(v1: Float), Some(v2: Double)) => Some(v1 <= v2)
                
                case (Some(v1: Double), Some(v2: Byte)) => Some(v1 <= v2)
                case (Some(v1: Double), Some(v2: Short)) => Some(v1 <= v2)
                case (Some(v1: Double), Some(v2: Int)) => Some(v1 <= v2)
                case (Some(v1: Double), Some(v2: Long)) => Some(v1 <= v2)
                case (Some(v1: Double), Some(v2: Float)) => Some(v1 <= v2)
                case (Some(v1: Double), Some(v2: Double)) => Some(v1 <= v2)
                
                case _ => None
            }
        }
    }
    
    final case class Grater(value1: OrderedValue[_], value2: OrderedValue[_]) extends BooleanValue {
        override def getValue(implicit entityMap: Map[Int, Entity]): Option[Boolean] = {
            (value1.getValue, value2.getValue) match {
                case (Some(v1: Byte), Some(v2: Byte)) => Some(v1 > v2)
                case (Some(v1: Byte), Some(v2: Short)) => Some(v1 > v2)
                case (Some(v1: Byte), Some(v2: Int)) => Some(v1 > v2)
                case (Some(v1: Byte), Some(v2: Long)) => Some(v1 > v2)
                case (Some(v1: Byte), Some(v2: Float)) => Some(v1 > v2)
                case (Some(v1: Byte), Some(v2: Double)) => Some(v1 > v2)
                
                case (Some(v1: Short), Some(v2: Byte)) => Some(v1 > v2)
                case (Some(v1: Short), Some(v2: Short)) => Some(v1 > v2)
                case (Some(v1: Short), Some(v2: Int)) => Some(v1 > v2)
                case (Some(v1: Short), Some(v2: Long)) => Some(v1 > v2)
                case (Some(v1: Short), Some(v2: Float)) => Some(v1 > v2)
                case (Some(v1: Short), Some(v2: Double)) => Some(v1 > v2)
                
                case (Some(v1: Int), Some(v2: Byte)) => Some(v1 > v2)
                case (Some(v1: Int), Some(v2: Short)) => Some(v1 > v2)
                case (Some(v1: Int), Some(v2: Int)) => Some(v1 > v2)
                case (Some(v1: Int), Some(v2: Long)) => Some(v1 > v2)
                case (Some(v1: Int), Some(v2: Float)) => Some(v1 > v2)
                case (Some(v1: Int), Some(v2: Double)) => Some(v1 > v2)
                
                case (Some(v1: Long), Some(v2: Byte)) => Some(v1 > v2)
                case (Some(v1: Long), Some(v2: Short)) => Some(v1 > v2)
                case (Some(v1: Long), Some(v2: Int)) => Some(v1 > v2)
                case (Some(v1: Long), Some(v2: Long)) => Some(v1 > v2)
                case (Some(v1: Long), Some(v2: Float)) => Some(v1 > v2)
                case (Some(v1: Long), Some(v2: Double)) => Some(v1 > v2)
                
                case (Some(v1: Float), Some(v2: Byte)) => Some(v1 > v2)
                case (Some(v1: Float), Some(v2: Short)) => Some(v1 > v2)
                case (Some(v1: Float), Some(v2: Int)) => Some(v1 > v2)
                case (Some(v1: Float), Some(v2: Long)) => Some(v1 > v2)
                case (Some(v1: Float), Some(v2: Float)) => Some(v1 > v2)
                case (Some(v1: Float), Some(v2: Double)) => Some(v1 > v2)
                
                case (Some(v1: Double), Some(v2: Byte)) => Some(v1 > v2)
                case (Some(v1: Double), Some(v2: Short)) => Some(v1 > v2)
                case (Some(v1: Double), Some(v2: Int)) => Some(v1 > v2)
                case (Some(v1: Double), Some(v2: Long)) => Some(v1 > v2)
                case (Some(v1: Double), Some(v2: Float)) => Some(v1 > v2)
                case (Some(v1: Double), Some(v2: Double)) => Some(v1 > v2)
                
                case _ => None
            }
        }
    }
    
    final case class GraterEqual(value1: OrderedValue[_], value2: OrderedValue[_]) extends BooleanValue {
        override def getValue(implicit entityMap: Map[Int, Entity]): Option[Boolean] = {
            (value1.getValue, value2.getValue) match {
                case (Some(v1: Byte), Some(v2: Byte)) => Some(v1 >= v2)
                case (Some(v1: Byte), Some(v2: Short)) => Some(v1 >= v2)
                case (Some(v1: Byte), Some(v2: Int)) => Some(v1 >= v2)
                case (Some(v1: Byte), Some(v2: Long)) => Some(v1 >= v2)
                case (Some(v1: Byte), Some(v2: Float)) => Some(v1 >= v2)
                case (Some(v1: Byte), Some(v2: Double)) => Some(v1 >= v2)
                
                case (Some(v1: Short), Some(v2: Byte)) => Some(v1 >= v2)
                case (Some(v1: Short), Some(v2: Short)) => Some(v1 >= v2)
                case (Some(v1: Short), Some(v2: Int)) => Some(v1 >= v2)
                case (Some(v1: Short), Some(v2: Long)) => Some(v1 >= v2)
                case (Some(v1: Short), Some(v2: Float)) => Some(v1 >= v2)
                case (Some(v1: Short), Some(v2: Double)) => Some(v1 >= v2)
                
                case (Some(v1: Int), Some(v2: Byte)) => Some(v1 >= v2)
                case (Some(v1: Int), Some(v2: Short)) => Some(v1 >= v2)
                case (Some(v1: Int), Some(v2: Int)) => Some(v1 >= v2)
                case (Some(v1: Int), Some(v2: Long)) => Some(v1 >= v2)
                case (Some(v1: Int), Some(v2: Float)) => Some(v1 >= v2)
                case (Some(v1: Int), Some(v2: Double)) => Some(v1 >= v2)
                
                case (Some(v1: Long), Some(v2: Byte)) => Some(v1 >= v2)
                case (Some(v1: Long), Some(v2: Short)) => Some(v1 >= v2)
                case (Some(v1: Long), Some(v2: Int)) => Some(v1 >= v2)
                case (Some(v1: Long), Some(v2: Long)) => Some(v1 >= v2)
                case (Some(v1: Long), Some(v2: Float)) => Some(v1 >= v2)
                case (Some(v1: Long), Some(v2: Double)) => Some(v1 >= v2)
                
                case (Some(v1: Float), Some(v2: Byte)) => Some(v1 >= v2)
                case (Some(v1: Float), Some(v2: Short)) => Some(v1 >= v2)
                case (Some(v1: Float), Some(v2: Int)) => Some(v1 >= v2)
                case (Some(v1: Float), Some(v2: Long)) => Some(v1 >= v2)
                case (Some(v1: Float), Some(v2: Float)) => Some(v1 >= v2)
                case (Some(v1: Float), Some(v2: Double)) => Some(v1 >= v2)
                
                case (Some(v1: Double), Some(v2: Byte)) => Some(v1 >= v2)
                case (Some(v1: Double), Some(v2: Short)) => Some(v1 >= v2)
                case (Some(v1: Double), Some(v2: Int)) => Some(v1 >= v2)
                case (Some(v1: Double), Some(v2: Long)) => Some(v1 >= v2)
                case (Some(v1: Double), Some(v2: Float)) => Some(v1 >= v2)
                case (Some(v1: Double), Some(v2: Double)) => Some(v1 >= v2)
                
                case _ => None
            }
        }
    }
    
}