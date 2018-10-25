package value.v2

import entity.Entity
import value.v2.DoubleValue._

import scala.language.implicitConversions

private[value] abstract class DoubleValue extends Value[Double] with NumericValue[Double] with OrderedValue[Double] {
    final def unary_+ : DoubleValue = this
    
    final def unary_- : DoubleValue = DoubleNegate(this)
    
    // add
    final def +(that: ByteValue): DoubleValue = DoubleAdd(this, that)
    
    final def +(that: ShortValue): DoubleValue = DoubleAdd(this, that)
    
    final def +(that: IntValue): DoubleValue = DoubleAdd(this, that)
    
    final def +(that: LongValue): DoubleValue = DoubleAdd(this, that)
    
    final def +(that: FloatValue): DoubleValue = DoubleAdd(this, that)
    
    final def +(that: DoubleValue): DoubleValue = DoubleAdd(this, that)
    
    // subtract
    final def -(that: ByteValue): DoubleValue = DoubleSubtract(this, that)
    
    final def -(that: ShortValue): DoubleValue = DoubleSubtract(this, that)
    
    final def -(that: IntValue): DoubleValue = DoubleSubtract(this, that)
    
    final def -(that: LongValue): DoubleValue = DoubleSubtract(this, that)
    
    final def -(that: FloatValue): DoubleValue = DoubleSubtract(this, that)
    
    final def -(that: DoubleValue): DoubleValue = DoubleSubtract(this, that)
    
    // multiply
    final def *(that: ByteValue): DoubleValue = DoubleMultiply(this, that)
    
    final def *(that: ShortValue): DoubleValue = DoubleMultiply(this, that)
    
    final def *(that: IntValue): DoubleValue = DoubleMultiply(this, that)
    
    final def *(that: LongValue): DoubleValue = DoubleMultiply(this, that)
    
    final def *(that: FloatValue): DoubleValue = DoubleMultiply(this, that)
    
    final def *(that: DoubleValue): DoubleValue = DoubleMultiply(this, that)
    
    // divide
    final def /(that: ByteValue): DoubleValue = DoubleDivide(this, that)
    
    final def /(that: ShortValue): DoubleValue = DoubleDivide(this, that)
    
    final def /(that: IntValue): DoubleValue = DoubleDivide(this, that)
    
    final def /(that: LongValue): DoubleValue = DoubleDivide(this, that)
    
    final def /(that: FloatValue): DoubleValue = DoubleDivide(this, that)
    
    final def /(that: DoubleValue): DoubleValue = DoubleDivide(this, that)
}

object DoubleValue {
    implicit def d2V(value: Double): DoubleValue = DoubleConstant(value)
    
    implicit class D2V(value: Double) {
        def toValue: DoubleValue = value
        
        def toByteValue: ByteValue = value.toByte
        
        def toShortValue: ShortValue = value.toShort
        
        def toIntValue: IntValue = value.toInt
        
        def toLongValue: LongValue = value.toLong
        
        def toFloatValue: FloatValue = value.toFloat
        
        def toDoubleValue: DoubleValue = value.toDouble
        
        def toStringValue: StringValue = value.toString
    }
    
    final case class DoubleConstant(value: Double) extends DoubleValue {
        override def getValue(implicit entityMap: Map[Int, Entity]): Option[Double] = Some(value)
    }
    
    final case class DoubleNegate(value: DoubleValue) extends DoubleValue {
        override def getValue(implicit entityMap: Map[Int, Entity]): Option[Double] = {
            value.getValue match {
                case Some(v) => Some(-v)
                case _ => None
            }
        }
    }
    
    final case class DoubleAdd(value1: DoubleValue, value2: DoubleValue) extends DoubleValue {
        override def getValue(implicit entityMap: Map[Int, Entity]): Option[Double] = {
            (value1.getValue, value2.getValue) match {
                case (Some(v1), Some(v2)) => Some(v1 + v2)
                case _ => None
            }
        }
    }
    
    final case class DoubleSubtract(value1: DoubleValue, value2: DoubleValue) extends DoubleValue {
        override def getValue(implicit entityMap: Map[Int, Entity]): Option[Double] = {
            (value1.getValue, value2.getValue) match {
                case (Some(v1), Some(v2)) => Some(v1 - v2)
                case _ => None
            }
        }
    }
    
    final case class DoubleMultiply(value1: DoubleValue, value2: DoubleValue) extends DoubleValue {
        override def getValue(implicit entityMap: Map[Int, Entity]): Option[Double] = {
            (value1.getValue, value2.getValue) match {
                case (Some(v1), Some(v2)) => Some(v1 * v2)
                case _ => None
            }
        }
    }
    
    final case class DoubleDivide(value1: DoubleValue, value2: DoubleValue) extends DoubleValue {
        override def getValue(implicit entityMap: Map[Int, Entity]): Option[Double] = {
            (value1.getValue, value2.getValue) match {
                case (Some(v1), Some(v2)) => Some(v1 / v2)
                case _ => None
            }
        }
    }
    
    final case class NumericToDouble(value: NumericValue[_]) extends DoubleValue {
        override def getValue(implicit entityMap: Map[Int, Entity]): Option[Double] = {
            value.getValue match {
                case Some(v: Byte) => Some(v.toDouble)
                case Some(v: Short) => Some(v.toDouble)
                case Some(v: Int) => Some(v.toDouble)
                case Some(v: Long) => Some(v.toDouble)
                case Some(v: Float) => Some(v.toDouble)
                case Some(v: Double) => Some(v.toDouble)
                case _ => None
            }
        }
    }
    
}