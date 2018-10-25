package value.v2

import entity.Entity
import value.ValueImplicits._

private[value] abstract class Value[T] {
    def getValue(implicit entityMap: Map[Int, Entity]): Option[T]
    
    def ===(that: Value[_]): Value.BooleanValue = Value.Equals(this, that)
    
    def =!=(that: Value[_]): Value.BooleanValue = Value.Unequals(this, that)
}

object Value {
    def apply(value: Byte): ByteConstant = ByteConstant(value)
    
    def apply(value: Short): ShortConstant = ShortConstant(value)
    
    def apply(value: Int): IntConstant = IntConstant(value)
    
    def apply(value: Long): LongConstant = LongConstant(value)
    
    def apply(value: Float): FloatConstant = FloatConstant(value)
    
    def apply(value: Double): DoubleConstant = DoubleConstant(value)
    
    def apply(value: Boolean): BooleanConstant = BooleanConstant(value)
    
    def apply(value: Char): CharConstant = CharConstant(value)
    
    def apply(value: String): StringConstant = StringConstant(value)
    
//    sealed trait OrderedValue[T] extends Value[T] {
//        def <(that: OrderedValue[_]): Less = Less(this, that)
//
//        def <=(that: OrderedValue[_]): LessEqual = LessEqual(this, that)
//
//        def >(that: OrderedValue[_]): Grater = Grater(this, that)
//
//        def >=(that: OrderedValue[_]): GraterEqual = GraterEqual(this, that)
//    }
    
//    sealed trait NumericValue[T] extends Value[T]
    
    // byte classes
//    sealed abstract class ByteValue extends Value[Byte] with NumericValue[Byte] with OrderedValue[Byte] {
//        final def unary_+ : ByteValue = this
//
//        final def unary_- : ByteValue = ByteNegate(this)
//
//        // add
//        final def +(that: ByteValue): ByteValue = ByteAdd(this, that)
//
//        final def +(that: ShortValue): ShortValue = ShortAdd(this, that)
//
//        final def +(that: IntValue): IntValue = IntAdd(this, that)
//
//        final def +(that: LongValue): LongValue = LongAdd(this, that)
//
//        final def +(that: FloatValue): FloatValue = FloatAdd(this, that)
//
//        final def +(that: DoubleValue): DoubleValue = DoubleAdd(this, that)
//
//        // subtract
//        final def -(that: ByteValue): ByteValue = ByteSubtract(this, that)
//
//        final def -(that: ShortValue): ShortValue = ShortSubtract(this, that)
//
//        final def -(that: IntValue): IntValue = IntSubtract(this, that)
//
//        final def -(that: LongValue): LongValue = LongSubtract(this, that)
//
//        final def -(that: FloatValue): FloatValue = FloatSubtract(this, that)
//
//        final def -(that: DoubleValue): DoubleValue = DoubleSubtract(this, that)
//
//        // multiply
//        final def *(that: ByteValue): ByteValue = ByteMultiply(this, that)
//
//        final def *(that: ShortValue): ShortValue = ShortMultiply(this, that)
//
//        final def *(that: IntValue): IntValue = IntMultiply(this, that)
//
//        final def *(that: LongValue): LongValue = LongMultiply(this, that)
//
//        final def *(that: FloatValue): FloatValue = FloatMultiply(this, that)
//
//        final def *(that: DoubleValue): DoubleValue = DoubleMultiply(this, that)
//
//        // divide
//        final def /(that: ByteValue): ByteValue = ByteDivide(this, that)
//
//        final def /(that: ShortValue): ShortValue = ShortDivide(this, that)
//
//        final def /(that: IntValue): IntValue = IntDivide(this, that)
//
//        final def /(that: LongValue): LongValue = LongDivide(this, that)
//
//        final def /(that: FloatValue): FloatValue = FloatDivide(this, that)
//
//        final def /(that: DoubleValue): DoubleValue = DoubleDivide(this, that)
//
//        // modulo
//        final def %(that: ByteValue): ByteValue = ByteMod(this, that)
//
//        final def %(that: ShortValue): ShortValue = ShortMod(this, that)
//
//        final def %(that: IntValue): IntValue = IntMod(this, that)
//
//        final def %(that: LongValue): LongValue = LongMod(this, that)
//    }
//
//    final case class ByteConstant(value: Byte) extends ByteValue {
//        override def getValue(implicit entityMap: Map[Int, Entity]): Option[Byte] = Some(value)
//    }
//
//    final case class ByteNegate(value: ByteValue) extends ByteValue {
//        override def getValue(implicit entityMap: Map[Int, Entity]): Option[Byte] = {
//            value.getValue match {
//                case Some(v) => Some((-v).toByte)
//                case _ => None
//            }
//        }
//    }
//
//    final case class ByteAdd(value1: ByteValue, value2: ByteValue) extends ByteValue {
//        override def getValue(implicit entityMap: Map[Int, Entity]): Option[Byte] = {
//            (value1.getValue, value2.getValue) match {
//                case (Some(v1), Some(v2)) => Some((v1 + v2).toByte)
//                case _ => None
//            }
//        }
//    }
//
//    final case class ByteSubtract(value1: ByteValue, value2: ByteValue) extends ByteValue {
//        override def getValue(implicit entityMap: Map[Int, Entity]): Option[Byte] = {
//            (value1.getValue, value2.getValue) match {
//                case (Some(v1), Some(v2)) => Some((v1 - v2).toByte)
//                case _ => None
//            }
//        }
//    }
//
//    final case class ByteMultiply(value1: ByteValue, value2: ByteValue) extends ByteValue {
//        override def getValue(implicit entityMap: Map[Int, Entity]): Option[Byte] = {
//            (value1.getValue, value2.getValue) match {
//                case (Some(v1), Some(v2)) => Some((v1 * v2).toByte)
//                case _ => None
//            }
//        }
//    }
//
//    final case class ByteDivide(value1: ByteValue, value2: ByteValue) extends ByteValue {
//        override def getValue(implicit entityMap: Map[Int, Entity]): Option[Byte] = {
//            (value1.getValue, value2.getValue) match {
//                case (Some(v1), Some(v2)) => Some((v1 / v2).toByte)
//                case _ => None
//            }
//        }
//    }
//
//    final case class ByteMod(value1: ByteValue, value2: ByteValue) extends ByteValue {
//        override def getValue(implicit entityMap: Map[Int, Entity]): Option[Byte] = {
//            (value1.getValue, value2.getValue) match {
//                case (Some(v1), Some(v2)) => Some((v1 % v2).toByte)
//                case _ => None
//            }
//        }
//    }
    
    // short classes
//    sealed abstract class ShortValue extends Value[Short] with NumericValue[Short] with OrderedValue[Short] {
//        final def unary_+ : ShortValue = this
//
//        final def unary_- : ShortValue = ShortNegate(this)
//
//        // add
//        final def +(that: ByteValue): ShortValue = ShortAdd(this, that)
//
//        final def +(that: ShortValue): ShortValue = ShortAdd(this, that)
//
//        final def +(that: IntValue): IntValue = IntAdd(this, that)
//
//        final def +(that: LongValue): LongValue = LongAdd(this, that)
//
//        final def +(that: FloatValue): FloatValue = FloatAdd(this, that)
//
//        final def +(that: DoubleValue): DoubleValue = DoubleAdd(this, that)
//
//        // subtract
//        final def -(that: ByteValue): ShortValue = ShortSubtract(this, that)
//
//        final def -(that: ShortValue): ShortValue = ShortSubtract(this, that)
//
//        final def -(that: IntValue): IntValue = IntSubtract(this, that)
//
//        final def -(that: LongValue): LongValue = LongSubtract(this, that)
//
//        final def -(that: FloatValue): FloatValue = FloatSubtract(this, that)
//
//        final def -(that: DoubleValue): DoubleValue = DoubleSubtract(this, that)
//
//        // multiply
//        final def *(that: ByteValue): ShortValue = ShortMultiply(this, that)
//
//        final def *(that: ShortValue): ShortValue = ShortMultiply(this, that)
//
//        final def *(that: IntValue): IntValue = IntMultiply(this, that)
//
//        final def *(that: LongValue): LongValue = LongMultiply(this, that)
//
//        final def *(that: FloatValue): FloatValue = FloatMultiply(this, that)
//
//        final def *(that: DoubleValue): DoubleValue = DoubleMultiply(this, that)
//
//        // divide
//        final def /(that: ByteValue): ShortValue = ShortDivide(this, that)
//
//        final def /(that: ShortValue): ShortValue = ShortDivide(this, that)
//
//        final def /(that: IntValue): IntValue = IntDivide(this, that)
//
//        final def /(that: LongValue): LongValue = LongDivide(this, that)
//
//        final def /(that: FloatValue): FloatValue = FloatDivide(this, that)
//
//        final def /(that: DoubleValue): DoubleValue = DoubleDivide(this, that)
//
//        // modulo
//        final def %(that: ByteValue): ShortValue = ShortMod(this, that)
//
//        final def %(that: ShortValue): ShortValue = ShortMod(this, that)
//
//        final def %(that: IntValue): IntValue = IntMod(this, that)
//
//        final def %(that: LongValue): LongValue = LongMod(this, that)
//    }
    
//    final case class ShortConstant(value: Short) extends ShortValue {
//        override def getValue(implicit entityMap: Map[Int, Entity]): Option[Short] = Some(value)
//    }
//
//    final case class ShortNegate(value: ShortValue) extends ShortValue {
//        override def getValue(implicit entityMap: Map[Int, Entity]): Option[Short] = {
//            value.getValue match {
//                case Some(v) => Some((-v).toShort)
//                case _ => None
//            }
//        }
//    }
//
//    final case class ShortAdd(value1: ShortValue, value2: ShortValue) extends ShortValue {
//        override def getValue(implicit entityMap: Map[Int, Entity]): Option[Short] = {
//            (value1.getValue, value2.getValue) match {
//                case (Some(v1), Some(v2)) => Some((v1 + v2).toShort)
//                case _ => None
//            }
//        }
//    }
//
//    final case class ShortSubtract(value1: ShortValue, value2: ShortValue) extends ShortValue {
//        override def getValue(implicit entityMap: Map[Int, Entity]): Option[Short] = {
//            (value1.getValue, value2.getValue) match {
//                case (Some(v1), Some(v2)) => Some((v1 - v2).toShort)
//                case _ => None
//            }
//        }
//    }
//
//    final case class ShortMultiply(value1: ShortValue, value2: ShortValue) extends ShortValue {
//        override def getValue(implicit entityMap: Map[Int, Entity]): Option[Short] = {
//            (value1.getValue, value2.getValue) match {
//                case (Some(v1), Some(v2)) => Some((v1 * v2).toShort)
//                case _ => None
//            }
//        }
//    }
//
//    final case class ShortDivide(value1: ShortValue, value2: ShortValue) extends ShortValue {
//        override def getValue(implicit entityMap: Map[Int, Entity]): Option[Short] = {
//            (value1.getValue, value2.getValue) match {
//                case (Some(v1), Some(v2)) => Some((v1 / v2).toShort)
//                case _ => None
//            }
//        }
//    }
//
//    final case class ShortMod(value1: ShortValue, value2: ShortValue) extends ShortValue {
//        override def getValue(implicit entityMap: Map[Int, Entity]): Option[Short] = {
//            (value1.getValue, value2.getValue) match {
//                case (Some(v1), Some(v2)) => Some((v1 % v2).toShort)
//                case _ => None
//            }
//        }
//    }
    
    // int classes
//    sealed abstract class IntValue extends Value[Int] with NumericValue[Int] with OrderedValue[Int] {
//        final def unary_+ : IntValue = this
//
//        final def unary_- : IntValue = IntNegate(this)
//
//        // add
//        final def +(that: ByteValue): IntValue = IntAdd(this, that)
//
//        final def +(that: ShortValue): IntValue = IntAdd(this, that)
//
//        final def +(that: IntValue): IntValue = IntAdd(this, that)
//
//        final def +(that: LongValue): LongValue = LongAdd(this, that)
//
//        final def +(that: FloatValue): FloatValue = FloatAdd(this, that)
//
//        final def +(that: DoubleValue): DoubleValue = DoubleAdd(this, that)
//
//        // subtract
//        final def -(that: ByteValue): IntValue = IntSubtract(this, that)
//
//        final def -(that: ShortValue): IntValue = IntSubtract(this, that)
//
//        final def -(that: IntValue): IntValue = IntSubtract(this, that)
//
//        final def -(that: LongValue): LongValue = LongSubtract(this, that)
//
//        final def -(that: FloatValue): FloatValue = FloatSubtract(this, that)
//
//        final def -(that: DoubleValue): DoubleValue = DoubleSubtract(this, that)
//
//        // multiply
//        final def *(that: ByteValue): IntValue = IntMultiply(this, that)
//
//        final def *(that: ShortValue): IntValue = IntMultiply(this, that)
//
//        final def *(that: IntValue): IntValue = IntMultiply(this, that)
//
//        final def *(that: LongValue): LongValue = LongMultiply(this, that)
//
//        final def *(that: FloatValue): FloatValue = FloatMultiply(this, that)
//
//        final def *(that: DoubleValue): DoubleValue = DoubleMultiply(this, that)
//
//        // divide
//        final def /(that: ByteValue): IntValue = IntDivide(this, that)
//
//        final def /(that: ShortValue): IntValue = IntDivide(this, that)
//
//        final def /(that: IntValue): IntValue = IntDivide(this, that)
//
//        final def /(that: LongValue): LongValue = LongDivide(this, that)
//
//        final def /(that: FloatValue): FloatValue = FloatDivide(this, that)
//
//        final def /(that: DoubleValue): DoubleValue = DoubleDivide(this, that)
//
//        // modulo
//        final def %(that: ByteValue): IntValue = IntMod(this, that)
//
//        final def %(that: ShortValue): IntValue = IntMod(this, that)
//
//        final def %(that: IntValue): IntValue = IntMod(this, that)
//
//        final def %(that: LongValue): LongValue = LongMod(this, that)
//    }
//
//    final case class IntConstant(value: Int) extends IntValue {
//        override def getValue(implicit entityMap: Map[Int, Entity]): Option[Int] = Some(value)
//    }
//
//    final case class IntNegate(value: IntValue) extends IntValue {
//        override def getValue(implicit entityMap: Map[Int, Entity]): Option[Int] = {
//            value.getValue match {
//                case Some(v) => Some(-v)
//                case _ => None
//            }
//        }
//    }
//
//    final case class IntAdd(value1: IntValue, value2: IntValue) extends IntValue {
//        override def getValue(implicit entityMap: Map[Int, Entity]): Option[Int] = {
//            (value1.getValue, value2.getValue) match {
//                case (Some(v1), Some(v2)) => Some(v1 + v2)
//                case _ => None
//            }
//        }
//    }
//
//    final case class IntSubtract(value1: IntValue, value2: IntValue) extends IntValue {
//        override def getValue(implicit entityMap: Map[Int, Entity]): Option[Int] = {
//            (value1.getValue, value2.getValue) match {
//                case (Some(v1), Some(v2)) => Some(v1 - v2)
//                case _ => None
//            }
//        }
//    }
//
//    final case class IntMultiply(value1: IntValue, value2: IntValue) extends IntValue {
//        override def getValue(implicit entityMap: Map[Int, Entity]): Option[Int] = {
//            (value1.getValue, value2.getValue) match {
//                case (Some(v1), Some(v2)) => Some(v1 * v2)
//                case _ => None
//            }
//        }
//    }
//
//    final case class IntDivide(value1: IntValue, value2: IntValue) extends IntValue {
//        override def getValue(implicit entityMap: Map[Int, Entity]): Option[Int] = {
//            (value1.getValue, value2.getValue) match {
//                case (Some(v1), Some(v2)) => Some(v1 / v2)
//                case _ => None
//            }
//        }
//    }
//
//    final case class IntMod(value1: IntValue, value2: IntValue) extends IntValue {
//        override def getValue(implicit entityMap: Map[Int, Entity]): Option[Int] = {
//            (value1.getValue, value2.getValue) match {
//                case (Some(v1), Some(v2)) => Some(v1 % v2)
//                case _ => None
//            }
//        }
//    }
    
    // long classes
//    sealed abstract class LongValue extends Value[Long] with NumericValue[Long] with OrderedValue[Long] {
//        final def unary_+ : LongValue = this
//
//        final def unary_- : LongValue = LongNegate(this)
//
//        // add
//        final def +(that: ByteValue): LongValue = LongAdd(this, that)
//
//        final def +(that: ShortValue): LongValue = LongAdd(this, that)
//
//        final def +(that: IntValue): LongValue = LongAdd(this, that)
//
//        final def +(that: LongValue): LongValue = LongAdd(this, that)
//
//        final def +(that: FloatValue): FloatValue = FloatAdd(this, that)
//
//        final def +(that: DoubleValue): DoubleValue = DoubleAdd(this, that)
//
//        // subtract
//        final def -(that: ByteValue): LongValue = LongSubtract(this, that)
//
//        final def -(that: ShortValue): LongValue = LongSubtract(this, that)
//
//        final def -(that: IntValue): LongValue = LongSubtract(this, that)
//
//        final def -(that: LongValue): LongValue = LongSubtract(this, that)
//
//        final def -(that: FloatValue): FloatValue = FloatSubtract(this, that)
//
//        final def -(that: DoubleValue): DoubleValue = DoubleSubtract(this, that)
//
//        // multiply
//        final def *(that: ByteValue): LongValue = LongMultiply(this, that)
//
//        final def *(that: ShortValue): LongValue = LongMultiply(this, that)
//
//        final def *(that: IntValue): LongValue = LongMultiply(this, that)
//
//        final def *(that: LongValue): LongValue = LongMultiply(this, that)
//
//        final def *(that: FloatValue): FloatValue = FloatMultiply(this, that)
//
//        final def *(that: DoubleValue): DoubleValue = DoubleMultiply(this, that)
//
//
//        // divide
//        final def /(that: ByteValue): LongValue = LongDivide(this, that)
//
//        final def /(that: ShortValue): LongValue = LongDivide(this, that)
//
//        final def /(that: IntValue): LongValue = LongDivide(this, that)
//
//        final def /(that: LongValue): LongValue = LongDivide(this, that)
//
//        final def /(that: FloatValue): FloatValue = FloatDivide(this, that)
//
//        final def /(that: DoubleValue): DoubleValue = DoubleDivide(this, that)
//
//        // modulo
//        final def %(that: ByteValue): LongValue = LongMod(this, that)
//
//        final def %(that: ShortValue): LongValue = LongMod(this, that)
//
//        final def %(that: IntValue): LongValue = LongMod(this, that)
//
//        final def %(that: LongValue): LongValue = LongMod(this, that)
//    }
//
//    final case class LongConstant(value: Long) extends LongValue {
//        override def getValue(implicit entityMap: Map[Int, Entity]): Option[Long] = Some(value)
//    }
//
//    final case class LongNegate(value: LongValue) extends LongValue {
//        override def getValue(implicit entityMap: Map[Int, Entity]): Option[Long] = {
//            value.getValue match {
//                case Some(v) => Some(-v)
//                case _ => None
//            }
//        }
//    }
//
//    final case class LongAdd(value1: LongValue, value2: LongValue) extends LongValue {
//        override def getValue(implicit entityMap: Map[Int, Entity]): Option[Long] = {
//            (value1.getValue, value2.getValue) match {
//                case (Some(v1), Some(v2)) => Some(v1 + v2)
//                case _ => None
//            }
//        }
//    }
//
//    final case class LongSubtract(value1: LongValue, value2: LongValue) extends LongValue {
//        override def getValue(implicit entityMap: Map[Int, Entity]): Option[Long] = {
//            (value1.getValue, value2.getValue) match {
//                case (Some(v1), Some(v2)) => Some(v1 - v2)
//                case _ => None
//            }
//        }
//    }
//
//    final case class LongMultiply(value1: LongValue, value2: LongValue) extends LongValue {
//        override def getValue(implicit entityMap: Map[Int, Entity]): Option[Long] = {
//            (value1.getValue, value2.getValue) match {
//                case (Some(v1), Some(v2)) => Some(v1 * v2)
//                case _ => None
//            }
//        }
//    }
//
//    final case class LongDivide(value1: LongValue, value2: LongValue) extends LongValue {
//        override def getValue(implicit entityMap: Map[Int, Entity]): Option[Long] = {
//            (value1.getValue, value2.getValue) match {
//                case (Some(v1), Some(v2)) => Some(v1 / v2)
//                case _ => None
//            }
//        }
//    }
//
//    final case class LongMod(value1: LongValue, value2: LongValue) extends LongValue {
//        override def getValue(implicit entityMap: Map[Int, Entity]): Option[Long] = {
//            (value1.getValue, value2.getValue) match {
//                case (Some(v1), Some(v2)) => Some(v1 % v2)
//                case _ => None
//            }
//        }
//    }
    
    // float classes
//    sealed abstract class FloatValue extends Value[Float] with NumericValue[Float] with OrderedValue[Float] {
//        final def unary_+ : FloatValue = this
//
//        final def unary_- : FloatValue = FloatNegate(this)
//
//        // add
//        final def +(that: ByteValue): FloatValue = FloatAdd(this, that)
//
//        final def +(that: ShortValue): FloatValue = FloatAdd(this, that)
//
//        final def +(that: IntValue): FloatValue = FloatAdd(this, that)
//
//        final def +(that: LongValue): FloatValue = FloatAdd(this, that)
//
//        final def +(that: FloatValue): FloatValue = FloatAdd(this, that)
//
//        final def +(that: DoubleValue): DoubleValue = DoubleAdd(this, that)
//
//        // subtract
//        final def -(that: ByteValue): FloatValue = FloatSubtract(this, that)
//
//        final def -(that: ShortValue): FloatValue = FloatSubtract(this, that)
//
//        final def -(that: IntValue): FloatValue = FloatSubtract(this, that)
//
//        final def -(that: LongValue): FloatValue = FloatSubtract(this, that)
//
//        final def -(that: FloatValue): FloatValue = FloatSubtract(this, that)
//
//        final def -(that: DoubleValue): DoubleValue = DoubleSubtract(this, that)
//
//        // multiply
//        final def *(that: ByteValue): FloatValue = FloatMultiply(this, that)
//
//        final def *(that: ShortValue): FloatValue = FloatMultiply(this, that)
//
//        final def *(that: IntValue): FloatValue = FloatMultiply(this, that)
//
//        final def *(that: LongValue): FloatValue = FloatMultiply(this, that)
//
//        final def *(that: FloatValue): FloatValue = FloatMultiply(this, that)
//
//        final def *(that: DoubleValue): DoubleValue = DoubleMultiply(this, that)
//
//        // divide
//        final def /(that: ByteValue): FloatValue = FloatDivide(this, that)
//
//        final def /(that: ShortValue): FloatValue = FloatDivide(this, that)
//
//        final def /(that: IntValue): FloatValue = FloatDivide(this, that)
//
//        final def /(that: LongValue): FloatValue = FloatDivide(this, that)
//
//        final def /(that: FloatValue): FloatValue = FloatDivide(this, that)
//
//        final def /(that: DoubleValue): DoubleValue = DoubleDivide(this, that)
//    }
//
//    final case class FloatConstant(value: Float) extends FloatValue {
//        override def getValue(implicit entityMap: Map[Int, Entity]): Option[Float] = Some(value)
//    }
//
//    final case class FloatNegate(value: FloatValue) extends FloatValue {
//        override def getValue(implicit entityMap: Map[Int, Entity]): Option[Float] = {
//            value.getValue match {
//                case Some(v) => Some(-v)
//                case _ => None
//            }
//        }
//    }
//
//    final case class FloatAdd(value1: FloatValue, value2: FloatValue) extends FloatValue {
//        override def getValue(implicit entityMap: Map[Int, Entity]): Option[Float] = {
//            (value1.getValue, value2.getValue) match {
//                case (Some(v1), Some(v2)) => Some(v1 + v2)
//                case _ => None
//            }
//        }
//    }
//
//    final case class FloatSubtract(value1: FloatValue, value2: FloatValue) extends FloatValue {
//        override def getValue(implicit entityMap: Map[Int, Entity]): Option[Float] = {
//            (value1.getValue, value2.getValue) match {
//                case (Some(v1), Some(v2)) => Some(v1 - v2)
//                case _ => None
//            }
//        }
//    }
//
//    final case class FloatMultiply(value1: FloatValue, value2: FloatValue) extends FloatValue {
//        override def getValue(implicit entityMap: Map[Int, Entity]): Option[Float] = {
//            (value1.getValue, value2.getValue) match {
//                case (Some(v1), Some(v2)) => Some(v1 * v2)
//                case _ => None
//            }
//        }
//    }
//
//    final case class FloatDivide(value1: FloatValue, value2: FloatValue) extends FloatValue {
//        override def getValue(implicit entityMap: Map[Int, Entity]): Option[Float] = {
//            (value1.getValue, value2.getValue) match {
//                case (Some(v1), Some(v2)) => Some(v1 / v2)
//                case _ => None
//            }
//        }
//    }
    
    // double classes
//    sealed abstract class DoubleValue extends Value[Double] with NumericValue[Double] with OrderedValue[Double] {
//        final def unary_+ : DoubleValue = this
//
//        final def unary_- : DoubleValue = DoubleNegate(this)
//
//        // add
//        final def +(that: ByteValue): DoubleValue = DoubleAdd(this, that)
//
//        final def +(that: ShortValue): DoubleValue = DoubleAdd(this, that)
//
//        final def +(that: IntValue): DoubleValue = DoubleAdd(this, that)
//
//        final def +(that: LongValue): DoubleValue = DoubleAdd(this, that)
//
//        final def +(that: FloatValue): DoubleValue = DoubleAdd(this, that)
//
//        final def +(that: DoubleValue): DoubleValue = DoubleAdd(this, that)
//
//        // subtract
//        final def -(that: ByteValue): DoubleValue = DoubleSubtract(this, that)
//
//        final def -(that: ShortValue): DoubleValue = DoubleSubtract(this, that)
//
//        final def -(that: IntValue): DoubleValue = DoubleSubtract(this, that)
//
//        final def -(that: LongValue): DoubleValue = DoubleSubtract(this, that)
//
//        final def -(that: FloatValue): DoubleValue = DoubleSubtract(this, that)
//
//        final def -(that: DoubleValue): DoubleValue = DoubleSubtract(this, that)
//
//        // multiply
//        final def *(that: ByteValue): DoubleValue = DoubleMultiply(this, that)
//
//        final def *(that: ShortValue): DoubleValue = DoubleMultiply(this, that)
//
//        final def *(that: IntValue): DoubleValue = DoubleMultiply(this, that)
//
//        final def *(that: LongValue): DoubleValue = DoubleMultiply(this, that)
//
//        final def *(that: FloatValue): DoubleValue = DoubleMultiply(this, that)
//
//        final def *(that: DoubleValue): DoubleValue = DoubleMultiply(this, that)
//
//        // divide
//        final def /(that: ByteValue): DoubleValue = DoubleDivide(this, that)
//
//        final def /(that: ShortValue): DoubleValue = DoubleDivide(this, that)
//
//        final def /(that: IntValue): DoubleValue = DoubleDivide(this, that)
//
//        final def /(that: LongValue): DoubleValue = DoubleDivide(this, that)
//
//        final def /(that: FloatValue): DoubleValue = DoubleDivide(this, that)
//
//        final def /(that: DoubleValue): DoubleValue = DoubleDivide(this, that)
//    }
//
//    final case class DoubleConstant(value: Double) extends DoubleValue {
//        override def getValue(implicit entityMap: Map[Int, Entity]): Option[Double] = Some(value)
//    }
//
//    final case class DoubleNegate(value: DoubleValue) extends DoubleValue {
//        override def getValue(implicit entityMap: Map[Int, Entity]): Option[Double] = {
//            value.getValue match {
//                case Some(v) => Some(-v)
//                case _ => None
//            }
//        }
//    }
//
//    final case class DoubleAdd(value1: DoubleValue, value2: DoubleValue) extends DoubleValue {
//        override def getValue(implicit entityMap: Map[Int, Entity]): Option[Double] = {
//            (value1.getValue, value2.getValue) match {
//                case (Some(v1), Some(v2)) => Some(v1 + v2)
//                case _ => None
//            }
//        }
//    }
//
//    final case class DoubleSubtract(value1: DoubleValue, value2: DoubleValue) extends DoubleValue {
//        override def getValue(implicit entityMap: Map[Int, Entity]): Option[Double] = {
//            (value1.getValue, value2.getValue) match {
//                case (Some(v1), Some(v2)) => Some(v1 - v2)
//                case _ => None
//            }
//        }
//    }
//
//    final case class DoubleMultiply(value1: DoubleValue, value2: DoubleValue) extends DoubleValue {
//        override def getValue(implicit entityMap: Map[Int, Entity]): Option[Double] = {
//            (value1.getValue, value2.getValue) match {
//                case (Some(v1), Some(v2)) => Some(v1 * v2)
//                case _ => None
//            }
//        }
//    }
//
//    final case class DoubleDivide(value1: DoubleValue, value2: DoubleValue) extends DoubleValue {
//        override def getValue(implicit entityMap: Map[Int, Entity]): Option[Double] = {
//            (value1.getValue, value2.getValue) match {
//                case (Some(v1), Some(v2)) => Some(v1 / v2)
//                case _ => None
//            }
//        }
//    }
    
    // boolean classes
    sealed abstract class BooleanValue extends Value[Boolean] {
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
                case value: CustomBooleanValue => NOT(value)
            }
        }
        
        final def &&(that: BooleanValue): BooleanValue = AND(this, that)
        
        final def !&&(that: BooleanValue): BooleanValue = NAND(this, that)
        
        final def ||(that: BooleanValue): BooleanValue = OR(this, that)
        
        final def !||(that: BooleanValue): BooleanValue = NOR(this, that)
        
        final def <>(that: BooleanValue): BooleanValue = XOR(this, that)
        
        final def !<>(that: BooleanValue): BooleanValue = XNOR(this, that)
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
    
    // char classes
    sealed abstract class CharValue extends Value[Char]
    
    final case class CharConstant(value: Char) extends CharValue {
        override def getValue(implicit entityMap: Map[Int, Entity]): Option[Char] = Some(value)
    }
    
    // string classes
    sealed abstract class StringValue extends Value[String] {
        final def +(that: StringValue): StringValue = Concatenate(this, that)
        
        final def length: IntValue = Length(this)
    }
    
    final case class StringConstant(value: String) extends StringValue {
        override def getValue(implicit entityMap: Map[Int, Entity]): Option[String] = Some(value)
    }
    
    final case class Concatenate(value1: StringValue, value2: StringValue) extends StringValue {
        override def getValue(implicit entityMap: Map[Int, Entity]): Option[String] = {
            (value1.getValue, value2.getValue) match {
                case (Some(v1), Some(v2)) => Some(v1 + v2)
                case _ => None
            }
        }
    }
    
    final case class Length(value: StringValue) extends IntValue {
        override def getValue(implicit entityMap: Map[Int, Entity]): Option[Int] = {
            value.getValue match {
                case Some(v) => Some(v.length)
                case _ => None
            }
        }
    }
    
    // comparators
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
    
    // converters
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
    
    final case class NumericToShort(value: NumericValue[_]) extends ShortValue {
        override def getValue(implicit entityMap: Map[Int, Entity]): Option[Short] = {
            value.getValue match {
                case Some(v: Byte) => Some(v.toShort)
                case Some(v: Short) => Some(v.toShort)
                case Some(v: Int) => Some(v.toShort)
                case Some(v: Long) => Some(v.toShort)
                case Some(v: Float) => Some(v.toShort)
                case Some(v: Double) => Some(v.toShort)
                case _ => None
            }
        }
    }
    
    final case class NumericToInt(value: NumericValue[_]) extends IntValue {
        override def getValue(implicit entityMap: Map[Int, Entity]): Option[Int] = {
            value.getValue match {
                case Some(v: Byte) => Some(v.toInt)
                case Some(v: Short) => Some(v.toInt)
                case Some(v: Int) => Some(v.toInt)
                case Some(v: Long) => Some(v.toInt)
                case Some(v: Float) => Some(v.toInt)
                case Some(v: Double) => Some(v.toInt)
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
    
    // custom abstract classes
    abstract class CustomValue[T] extends Value[T]
    
    abstract class CustomByteValue extends ByteValue
    
    abstract class CustomShortValue extends ShortValue
    
    abstract class CustomIntValue extends IntValue
    
    abstract class CustomLongValue extends LongValue
    
    abstract class CustomFloatValue extends FloatValue
    
    abstract class CustomDoubleValue extends DoubleValue
    
    abstract class CustomBooleanValue extends BooleanValue
    
    abstract class CustomCharValue extends CharValue
    
    abstract class CustomStringValue extends StringValue
    
}