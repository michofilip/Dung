package value.v2

import entity.Entity
import value.v2.DoubleValue._
import value.v2.FloatValue._

import scala.language.implicitConversions

sealed abstract class FloatValue extends Value[Float] with NumericValue[Float] with OrderedValue[Float] {
    final def unary_+ : FloatValue = this
    
    final def unary_- : FloatValue = FloatNegate(this)
    
    // add
    final def +(that: ByteValue): FloatValue = FloatAdd(this, that)
    
    final def +(that: ShortValue): FloatValue = FloatAdd(this, that)
    
    final def +(that: IntValue): FloatValue = FloatAdd(this, that)
    
    final def +(that: LongValue): FloatValue = FloatAdd(this, that)
    
    final def +(that: FloatValue): FloatValue = FloatAdd(this, that)
    
    final def +(that: DoubleValue): DoubleValue = DoubleAdd(this, that)
    
    // subtract
    final def -(that: ByteValue): FloatValue = FloatSubtract(this, that)
    
    final def -(that: ShortValue): FloatValue = FloatSubtract(this, that)
    
    final def -(that: IntValue): FloatValue = FloatSubtract(this, that)
    
    final def -(that: LongValue): FloatValue = FloatSubtract(this, that)
    
    final def -(that: FloatValue): FloatValue = FloatSubtract(this, that)
    
    final def -(that: DoubleValue): DoubleValue = DoubleSubtract(this, that)
    
    // multiply
    final def *(that: ByteValue): FloatValue = FloatMultiply(this, that)
    
    final def *(that: ShortValue): FloatValue = FloatMultiply(this, that)
    
    final def *(that: IntValue): FloatValue = FloatMultiply(this, that)
    
    final def *(that: LongValue): FloatValue = FloatMultiply(this, that)
    
    final def *(that: FloatValue): FloatValue = FloatMultiply(this, that)
    
    final def *(that: DoubleValue): DoubleValue = DoubleMultiply(this, that)
    
    // divide
    final def /(that: ByteValue): FloatValue = FloatDivide(this, that)
    
    final def /(that: ShortValue): FloatValue = FloatDivide(this, that)
    
    final def /(that: IntValue): FloatValue = FloatDivide(this, that)
    
    final def /(that: LongValue): FloatValue = FloatDivide(this, that)
    
    final def /(that: FloatValue): FloatValue = FloatDivide(this, that)
    
    final def /(that: DoubleValue): DoubleValue = DoubleDivide(this, that)
}

object FloatValue {
    implicit def f2V(value: Float): FloatValue = FloatConstant(value)
    
    implicit class F2V(value: Float) {
        def toValue: FloatValue = value
        
        def toByteValue: ByteValue = value.toByte
        
        def toShortValue: ShortValue = value.toShort
        
        def toIntValue: IntValue = value.toInt
        
        def toLongValue: LongValue = value.toLong
        
        def toFloatValue: FloatValue = value.toFloat
        
        def toDoubleValue: DoubleValue = value.toDouble
        
        def toStringValue: StringValue = value.toString
    }
    
    final case class FloatConstant(value: Float) extends FloatValue {
        override def getValue(implicit entityMap: Map[Int, Entity]): Option[Float] = Some(value)
    }
    
    final case class FloatNegate(value: FloatValue) extends FloatValue {
        override def getValue(implicit entityMap: Map[Int, Entity]): Option[Float] = {
            value.getValue match {
                case Some(v) => Some(-v)
                case _ => None
            }
        }
    }
    
    final case class FloatAdd(value1: FloatValue, value2: FloatValue) extends FloatValue {
        override def getValue(implicit entityMap: Map[Int, Entity]): Option[Float] = {
            (value1.getValue, value2.getValue) match {
                case (Some(v1), Some(v2)) => Some(v1 + v2)
                case _ => None
            }
        }
    }
    
    final case class FloatSubtract(value1: FloatValue, value2: FloatValue) extends FloatValue {
        override def getValue(implicit entityMap: Map[Int, Entity]): Option[Float] = {
            (value1.getValue, value2.getValue) match {
                case (Some(v1), Some(v2)) => Some(v1 - v2)
                case _ => None
            }
        }
    }
    
    final case class FloatMultiply(value1: FloatValue, value2: FloatValue) extends FloatValue {
        override def getValue(implicit entityMap: Map[Int, Entity]): Option[Float] = {
            (value1.getValue, value2.getValue) match {
                case (Some(v1), Some(v2)) => Some(v1 * v2)
                case _ => None
            }
        }
    }
    
    final case class FloatDivide(value1: FloatValue, value2: FloatValue) extends FloatValue {
        override def getValue(implicit entityMap: Map[Int, Entity]): Option[Float] = {
            (value1.getValue, value2.getValue) match {
                case (Some(v1), Some(v2)) => Some(v1 / v2)
                case _ => None
            }
        }
    }
    
    final case class NumericToFloat(value: NumericValue[_]) extends FloatValue {
        override def getValue(implicit entityMap: Map[Int, Entity]): Option[Float] = {
            value.getValue match {
                case Some(v: Byte) => Some(v.toFloat)
                case Some(v: Short) => Some(v.toFloat)
                case Some(v: Int) => Some(v.toFloat)
                case Some(v: Long) => Some(v.toFloat)
                case Some(v: Float) => Some(v.toFloat)
                case Some(v: Double) => Some(v.toFloat)
                case _ => None
            }
        }
    }
    
}