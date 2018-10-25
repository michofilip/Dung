package value.v2

import entity.Entity

private[value] abstract class IntValue extends Value[Int] with NumericValue[Int] with OrderedValue[Int] {
    final def unary_+ : IntValue = this
    
    final def unary_- : IntValue = IntNegate(this)
    
    // add
    final def +(that: ByteValue): IntValue = IntAdd(this, that)
    
    final def +(that: ShortValue): IntValue = IntAdd(this, that)
    
    final def +(that: IntValue): IntValue = IntAdd(this, that)
    
    final def +(that: LongValue): LongValue = LongAdd(this, that)
    
    final def +(that: FloatValue): FloatValue = FloatAdd(this, that)
    
    final def +(that: DoubleValue): DoubleValue = DoubleAdd(this, that)
    
    // subtract
    final def -(that: ByteValue): IntValue = IntSubtract(this, that)
    
    final def -(that: ShortValue): IntValue = IntSubtract(this, that)
    
    final def -(that: IntValue): IntValue = IntSubtract(this, that)
    
    final def -(that: LongValue): LongValue = LongSubtract(this, that)
    
    final def -(that: FloatValue): FloatValue = FloatSubtract(this, that)
    
    final def -(that: DoubleValue): DoubleValue = DoubleSubtract(this, that)
    
    // multiply
    final def *(that: ByteValue): IntValue = IntMultiply(this, that)
    
    final def *(that: ShortValue): IntValue = IntMultiply(this, that)
    
    final def *(that: IntValue): IntValue = IntMultiply(this, that)
    
    final def *(that: LongValue): LongValue = LongMultiply(this, that)
    
    final def *(that: FloatValue): FloatValue = FloatMultiply(this, that)
    
    final def *(that: DoubleValue): DoubleValue = DoubleMultiply(this, that)
    
    // divide
    final def /(that: ByteValue): IntValue = IntDivide(this, that)
    
    final def /(that: ShortValue): IntValue = IntDivide(this, that)
    
    final def /(that: IntValue): IntValue = IntDivide(this, that)
    
    final def /(that: LongValue): LongValue = LongDivide(this, that)
    
    final def /(that: FloatValue): FloatValue = FloatDivide(this, that)
    
    final def /(that: DoubleValue): DoubleValue = DoubleDivide(this, that)
    
    // modulo
    final def %(that: ByteValue): IntValue = IntMod(this, that)
    
    final def %(that: ShortValue): IntValue = IntMod(this, that)
    
    final def %(that: IntValue): IntValue = IntMod(this, that)
    
    final def %(that: LongValue): LongValue = LongMod(this, that)
}

object IntValue {
    
    final case class IntConstant(value: Int) extends IntValue {
        override def getValue(implicit entityMap: Map[Int, Entity]): Option[Int] = Some(value)
    }
    
    final case class IntNegate(value: IntValue) extends IntValue {
        override def getValue(implicit entityMap: Map[Int, Entity]): Option[Int] = {
            value.getValue match {
                case Some(v) => Some(-v)
                case _ => None
            }
        }
    }
    
    final case class IntAdd(value1: IntValue, value2: IntValue) extends IntValue {
        override def getValue(implicit entityMap: Map[Int, Entity]): Option[Int] = {
            (value1.getValue, value2.getValue) match {
                case (Some(v1), Some(v2)) => Some(v1 + v2)
                case _ => None
            }
        }
    }
    
    final case class IntSubtract(value1: IntValue, value2: IntValue) extends IntValue {
        override def getValue(implicit entityMap: Map[Int, Entity]): Option[Int] = {
            (value1.getValue, value2.getValue) match {
                case (Some(v1), Some(v2)) => Some(v1 - v2)
                case _ => None
            }
        }
    }
    
    final case class IntMultiply(value1: IntValue, value2: IntValue) extends IntValue {
        override def getValue(implicit entityMap: Map[Int, Entity]): Option[Int] = {
            (value1.getValue, value2.getValue) match {
                case (Some(v1), Some(v2)) => Some(v1 * v2)
                case _ => None
            }
        }
    }
    
    final case class IntDivide(value1: IntValue, value2: IntValue) extends IntValue {
        override def getValue(implicit entityMap: Map[Int, Entity]): Option[Int] = {
            (value1.getValue, value2.getValue) match {
                case (Some(v1), Some(v2)) => Some(v1 / v2)
                case _ => None
            }
        }
    }
    
    final case class IntMod(value1: IntValue, value2: IntValue) extends IntValue {
        override def getValue(implicit entityMap: Map[Int, Entity]): Option[Int] = {
            (value1.getValue, value2.getValue) match {
                case (Some(v1), Some(v2)) => Some(v1 % v2)
                case _ => None
            }
        }
    }
    
}