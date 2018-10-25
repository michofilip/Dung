package value.v2

import entity.Entity
import value.v2.ByteValue._
import value.v2.DoubleValue._
import value.v2.FloatValue._
import value.v2.IntValue._
import value.v2.LongValue._
import value.v2.ShortValue._

import scala.language.implicitConversions

private[value] abstract class ByteValue extends Value[Byte] with NumericValue[Byte] with OrderedValue[Byte] {
    final def unary_+ : ByteValue = this
    
    final def unary_- : ByteValue = ByteNegate(this)
    
    // add
    final def +(that: ByteValue): ByteValue = ByteAdd(this, that)
    
    final def +(that: ShortValue): ShortValue = ShortAdd(this, that)
    
    final def +(that: IntValue): IntValue = IntAdd(this, that)
    
    final def +(that: LongValue): LongValue = LongAdd(this, that)
    
    final def +(that: FloatValue): FloatValue = FloatAdd(this, that)
    
    final def +(that: DoubleValue): DoubleValue = DoubleAdd(this, that)
    
    // subtract
    final def -(that: ByteValue): ByteValue = ByteSubtract(this, that)
    
    final def -(that: ShortValue): ShortValue = ShortSubtract(this, that)
    
    final def -(that: IntValue): IntValue = IntSubtract(this, that)
    
    final def -(that: LongValue): LongValue = LongSubtract(this, that)
    
    final def -(that: FloatValue): FloatValue = FloatSubtract(this, that)
    
    final def -(that: DoubleValue): DoubleValue = DoubleSubtract(this, that)
    
    // multiply
    final def *(that: ByteValue): ByteValue = ByteMultiply(this, that)
    
    final def *(that: ShortValue): ShortValue = ShortMultiply(this, that)
    
    final def *(that: IntValue): IntValue = IntMultiply(this, that)
    
    final def *(that: LongValue): LongValue = LongMultiply(this, that)
    
    final def *(that: FloatValue): FloatValue = FloatMultiply(this, that)
    
    final def *(that: DoubleValue): DoubleValue = DoubleMultiply(this, that)
    
    // divide
    final def /(that: ByteValue): ByteValue = ByteDivide(this, that)
    
    final def /(that: ShortValue): ShortValue = ShortDivide(this, that)
    
    final def /(that: IntValue): IntValue = IntDivide(this, that)
    
    final def /(that: LongValue): LongValue = LongDivide(this, that)
    
    final def /(that: FloatValue): FloatValue = FloatDivide(this, that)
    
    final def /(that: DoubleValue): DoubleValue = DoubleDivide(this, that)
    
    // modulo
    final def %(that: ByteValue): ByteValue = ByteMod(this, that)
    
    final def %(that: ShortValue): ShortValue = ShortMod(this, that)
    
    final def %(that: IntValue): IntValue = IntMod(this, that)
    
    final def %(that: LongValue): LongValue = LongMod(this, that)
}

object ByteValue {
    implicit def b2V(value: Byte): ByteValue = ByteConstant(value)
    
    implicit class B2V(value: Byte) {
        def toValue: ByteValue = value
        
        def toByteValue: ByteValue = value.toByte
        
        def toShortValue: ShortValue = value.toShort
        
        def toIntValue: IntValue = value.toInt
        
        def toLongValue: LongValue = value.toLong
        
        def toFloatValue: FloatValue = value.toFloat
        
        def toDoubleValue: DoubleValue = value.toDouble
        
        def toStringValue: StringValue = value.toString
    }
    
    final case class ByteConstant(value: Byte) extends ByteValue {
        override def getValue(implicit entityMap: Map[Int, Entity]): Option[Byte] = Some(value)
    }
    
    final case class ByteNegate(value: ByteValue) extends ByteValue {
        override def getValue(implicit entityMap: Map[Int, Entity]): Option[Byte] = {
            value.getValue match {
                case Some(v) => Some((-v).toByte)
                case _ => None
            }
        }
    }
    
    final case class ByteAdd(value1: ByteValue, value2: ByteValue) extends ByteValue {
        override def getValue(implicit entityMap: Map[Int, Entity]): Option[Byte] = {
            (value1.getValue, value2.getValue) match {
                case (Some(v1), Some(v2)) => Some((v1 + v2).toByte)
                case _ => None
            }
        }
    }
    
    final case class ByteSubtract(value1: ByteValue, value2: ByteValue) extends ByteValue {
        override def getValue(implicit entityMap: Map[Int, Entity]): Option[Byte] = {
            (value1.getValue, value2.getValue) match {
                case (Some(v1), Some(v2)) => Some((v1 - v2).toByte)
                case _ => None
            }
        }
    }
    
    final case class ByteMultiply(value1: ByteValue, value2: ByteValue) extends ByteValue {
        override def getValue(implicit entityMap: Map[Int, Entity]): Option[Byte] = {
            (value1.getValue, value2.getValue) match {
                case (Some(v1), Some(v2)) => Some((v1 * v2).toByte)
                case _ => None
            }
        }
    }
    
    final case class ByteDivide(value1: ByteValue, value2: ByteValue) extends ByteValue {
        override def getValue(implicit entityMap: Map[Int, Entity]): Option[Byte] = {
            (value1.getValue, value2.getValue) match {
                case (Some(v1), Some(v2)) => Some((v1 / v2).toByte)
                case _ => None
            }
        }
    }
    
    final case class ByteMod(value1: ByteValue, value2: ByteValue) extends ByteValue {
        override def getValue(implicit entityMap: Map[Int, Entity]): Option[Byte] = {
            (value1.getValue, value2.getValue) match {
                case (Some(v1), Some(v2)) => Some((v1 % v2).toByte)
                case _ => None
            }
        }
    }
    
    final case class NumericToByte(value: NumericValue[_]) extends ByteValue {
        override def getValue(implicit entityMap: Map[Int, Entity]): Option[Byte] = {
            value.getValue match {
                case Some(v: Byte) => Some(v.toByte)
                case Some(v: Short) => Some(v.toByte)
                case Some(v: Int) => Some(v.toByte)
                case Some(v: Long) => Some(v.toByte)
                case Some(v: Float) => Some(v.toByte)
                case Some(v: Double) => Some(v.toByte)
                case _ => None
            }
        }
    }
    
}