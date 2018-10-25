package value.v2

import entity.Entity

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
    
}