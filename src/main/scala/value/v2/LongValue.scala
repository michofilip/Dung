package value.v2

import entity.Entity

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
    
}