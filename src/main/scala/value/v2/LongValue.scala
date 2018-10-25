package value.v2

import entity.Entity
import value.v2.DoubleValue._
import value.v2.FloatValue._
import value.v2.LongValue._

import scala.language.implicitConversions

private[value] abstract class LongValue extends Value[Long] with NumericValue[Long] with OrderedValue[Long] {
    final def unary_+ : LongValue = this
    
    final def unary_- : LongValue = LongNegate(this)
    
    // add
    final def +(that: ByteValue): LongValue = LongAdd(this, that)
    
    final def +(that: ShortValue): LongValue = LongAdd(this, that)
    
    final def +(that: IntValue): LongValue = LongAdd(this, that)
    
    final def +(that: LongValue): LongValue = LongAdd(this, that)
    
    final def +(that: FloatValue): FloatValue = FloatAdd(this, that)
    
    final def +(that: DoubleValue): DoubleValue = DoubleAdd(this, that)
    
    // subtract
    final def -(that: ByteValue): LongValue = LongSubtract(this, that)
    
    final def -(that: ShortValue): LongValue = LongSubtract(this, that)
    
    final def -(that: IntValue): LongValue = LongSubtract(this, that)
    
    final def -(that: LongValue): LongValue = LongSubtract(this, that)
    
    final def -(that: FloatValue): FloatValue = FloatSubtract(this, that)
    
    final def -(that: DoubleValue): DoubleValue = DoubleSubtract(this, that)
    
    // multiply
    final def *(that: ByteValue): LongValue = LongMultiply(this, that)
    
    final def *(that: ShortValue): LongValue = LongMultiply(this, that)
    
    final def *(that: IntValue): LongValue = LongMultiply(this, that)
    
    final def *(that: LongValue): LongValue = LongMultiply(this, that)
    
    final def *(that: FloatValue): FloatValue = FloatMultiply(this, that)
    
    final def *(that: DoubleValue): DoubleValue = DoubleMultiply(this, that)
    
    
    // divide
    final def /(that: ByteValue): LongValue = LongDivide(this, that)
    
    final def /(that: ShortValue): LongValue = LongDivide(this, that)
    
    final def /(that: IntValue): LongValue = LongDivide(this, that)
    
    final def /(that: LongValue): LongValue = LongDivide(this, that)
    
    final def /(that: FloatValue): FloatValue = FloatDivide(this, that)
    
    final def /(that: DoubleValue): DoubleValue = DoubleDivide(this, that)
    
    // modulo
    final def %(that: ByteValue): LongValue = LongMod(this, that)
    
    final def %(that: ShortValue): LongValue = LongMod(this, that)
    
    final def %(that: IntValue): LongValue = LongMod(this, that)
    
    final def %(that: LongValue): LongValue = LongMod(this, that)
}

object LongValue {
    implicit def l2V(value: Long): LongValue = LongConstant(value)
    
    implicit class L2V(value: Long) {
        def toValue: LongValue = value
        
        def toByteValue: ByteValue = value.toByte
        
        def toShortValue: ShortValue = value.toShort
        
        def toIntValue: IntValue = value.toInt
        
        def toLongValue: LongValue = value.toLong
        
        def toFloatValue: FloatValue = value.toFloat
        
        def toDoubleValue: DoubleValue = value.toDouble
        
        def toStringValue: StringValue = value.toString
    }
    
    final case class LongConstant(value: Long) extends LongValue {
        override def getValue(implicit entityMap: Map[Int, Entity]): Option[Long] = Some(value)
    }
    
    final case class LongNegate(value: LongValue) extends LongValue {
        override def getValue(implicit entityMap: Map[Int, Entity]): Option[Long] = {
            value.getValue match {
                case Some(v) => Some(-v)
                case _ => None
            }
        }
    }
    
    final case class LongAdd(value1: LongValue, value2: LongValue) extends LongValue {
        override def getValue(implicit entityMap: Map[Int, Entity]): Option[Long] = {
            (value1.getValue, value2.getValue) match {
                case (Some(v1), Some(v2)) => Some(v1 + v2)
                case _ => None
            }
        }
    }
    
    final case class LongSubtract(value1: LongValue, value2: LongValue) extends LongValue {
        override def getValue(implicit entityMap: Map[Int, Entity]): Option[Long] = {
            (value1.getValue, value2.getValue) match {
                case (Some(v1), Some(v2)) => Some(v1 - v2)
                case _ => None
            }
        }
    }
    
    final case class LongMultiply(value1: LongValue, value2: LongValue) extends LongValue {
        override def getValue(implicit entityMap: Map[Int, Entity]): Option[Long] = {
            (value1.getValue, value2.getValue) match {
                case (Some(v1), Some(v2)) => Some(v1 * v2)
                case _ => None
            }
        }
    }
    
    final case class LongDivide(value1: LongValue, value2: LongValue) extends LongValue {
        override def getValue(implicit entityMap: Map[Int, Entity]): Option[Long] = {
            (value1.getValue, value2.getValue) match {
                case (Some(v1), Some(v2)) => Some(v1 / v2)
                case _ => None
            }
        }
    }
    
    final case class LongMod(value1: LongValue, value2: LongValue) extends LongValue {
        override def getValue(implicit entityMap: Map[Int, Entity]): Option[Long] = {
            (value1.getValue, value2.getValue) match {
                case (Some(v1), Some(v2)) => Some(v1 % v2)
                case _ => None
            }
        }
    }
    
    final case class NumericToLong(value: NumericValue[_]) extends LongValue {
        override def getValue(implicit entityMap: Map[Int, Entity]): Option[Long] = {
            value.getValue match {
                case Some(v: Byte) => Some(v.toLong)
                case Some(v: Short) => Some(v.toLong)
                case Some(v: Int) => Some(v.toLong)
                case Some(v: Long) => Some(v.toLong)
                case Some(v: Float) => Some(v.toLong)
                case Some(v: Double) => Some(v.toLong)
                case _ => None
            }
        }
    }
    
}