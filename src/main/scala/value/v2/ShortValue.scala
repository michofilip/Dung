package value.v2

import entity.Entity
import value.v2.ShortValue._

sealed abstract class ShortValue extends Value[Short] with NumericValue[Short] with OrderedValue[Short] {
    final def unary_+ : ShortValue = this
    
    final def unary_- : ShortValue = ShortNegate(this)
    
    // add
    final def +(that: ByteValue): ShortValue = ShortAdd(this, that)
    
    final def +(that: ShortValue): ShortValue = ShortAdd(this, that)
    
    final def +(that: IntValue): IntValue = IntAdd(this, that)
    
    final def +(that: LongValue): LongValue = LongAdd(this, that)
    
    final def +(that: FloatValue): FloatValue = FloatAdd(this, that)
    
    final def +(that: DoubleValue): DoubleValue = DoubleAdd(this, that)
    
    // subtract
    final def -(that: ByteValue): ShortValue = ShortSubtract(this, that)
    
    final def -(that: ShortValue): ShortValue = ShortSubtract(this, that)
    
    final def -(that: IntValue): IntValue = IntSubtract(this, that)
    
    final def -(that: LongValue): LongValue = LongSubtract(this, that)
    
    final def -(that: FloatValue): FloatValue = FloatSubtract(this, that)
    
    final def -(that: DoubleValue): DoubleValue = DoubleSubtract(this, that)
    
    // multiply
    final def *(that: ByteValue): ShortValue = ShortMultiply(this, that)
    
    final def *(that: ShortValue): ShortValue = ShortMultiply(this, that)
    
    final def *(that: IntValue): IntValue = IntMultiply(this, that)
    
    final def *(that: LongValue): LongValue = LongMultiply(this, that)
    
    final def *(that: FloatValue): FloatValue = FloatMultiply(this, that)
    
    final def *(that: DoubleValue): DoubleValue = DoubleMultiply(this, that)
    
    // divide
    final def /(that: ByteValue): ShortValue = ShortDivide(this, that)
    
    final def /(that: ShortValue): ShortValue = ShortDivide(this, that)
    
    final def /(that: IntValue): IntValue = IntDivide(this, that)
    
    final def /(that: LongValue): LongValue = LongDivide(this, that)
    
    final def /(that: FloatValue): FloatValue = FloatDivide(this, that)
    
    final def /(that: DoubleValue): DoubleValue = DoubleDivide(this, that)
    
    // modulo
    final def %(that: ByteValue): ShortValue = ShortMod(this, that)
    
    final def %(that: ShortValue): ShortValue = ShortMod(this, that)
    
    final def %(that: IntValue): IntValue = IntMod(this, that)
    
    final def %(that: LongValue): LongValue = LongMod(this, that)
}

object ShortValue {
    
    final case class ShortConstant(value: Short) extends ShortValue {
        override def getValue(implicit entityMap: Map[Int, Entity]): Option[Short] = Some(value)
    }
    
    final case class ShortNegate(value: ShortValue) extends ShortValue {
        override def getValue(implicit entityMap: Map[Int, Entity]): Option[Short] = {
            value.getValue match {
                case Some(v) => Some((-v).toShort)
                case _ => None
            }
        }
    }
    
    final case class ShortAdd(value1: ShortValue, value2: ShortValue) extends ShortValue {
        override def getValue(implicit entityMap: Map[Int, Entity]): Option[Short] = {
            (value1.getValue, value2.getValue) match {
                case (Some(v1), Some(v2)) => Some((v1 + v2).toShort)
                case _ => None
            }
        }
    }
    
    final case class ShortSubtract(value1: ShortValue, value2: ShortValue) extends ShortValue {
        override def getValue(implicit entityMap: Map[Int, Entity]): Option[Short] = {
            (value1.getValue, value2.getValue) match {
                case (Some(v1), Some(v2)) => Some((v1 - v2).toShort)
                case _ => None
            }
        }
    }
    
    final case class ShortMultiply(value1: ShortValue, value2: ShortValue) extends ShortValue {
        override def getValue(implicit entityMap: Map[Int, Entity]): Option[Short] = {
            (value1.getValue, value2.getValue) match {
                case (Some(v1), Some(v2)) => Some((v1 * v2).toShort)
                case _ => None
            }
        }
    }
    
    final case class ShortDivide(value1: ShortValue, value2: ShortValue) extends ShortValue {
        override def getValue(implicit entityMap: Map[Int, Entity]): Option[Short] = {
            (value1.getValue, value2.getValue) match {
                case (Some(v1), Some(v2)) => Some((v1 / v2).toShort)
                case _ => None
            }
        }
    }
    
    final case class ShortMod(value1: ShortValue, value2: ShortValue) extends ShortValue {
        override def getValue(implicit entityMap: Map[Int, Entity]): Option[Short] = {
            (value1.getValue, value2.getValue) match {
                case (Some(v1), Some(v2)) => Some((v1 % v2).toShort)
                case _ => None
            }
        }
    }
    
}
