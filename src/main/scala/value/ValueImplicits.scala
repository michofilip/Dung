package value

import value.Value._

import scala.language.implicitConversions

object ValueImplicits {
    implicit def b2v(value: Byte): ByteValue = ByteConstant(value)
    
    implicit def s2v(value: Short): ShortValue = ShortConstant(value)
    
    implicit def i2v(value: Int): IntValue = IntConstant(value)
    
    implicit def l2v(value: Long): LongValue = LongConstant(value)
    
    implicit def f2v(value: Float): FloatValue = FloatConstant(value)
    
    implicit def d2v(value: Double): DoubleValue = DoubleConstant(value)
    
    implicit def bool2v(value: Boolean): BooleanValue = BooleanConstant(value)
    
    implicit def char2v(value: Char): CharValue = CharConstant(value)
    
    implicit def str2v(value: String): StringValue = StringConstant(value)
    
    implicit class B2v(value: Byte) {
        def toValue: ByteValue = value
        
        def toByteValue: ByteValue = value.toByte
        
        def toShortValue: ShortValue = value.toShort
        
        def toIntValue: IntValue = value.toInt
        
        def toLongValue: LongValue = value.toLong
        
        def toFloatValue: FloatValue = value.toFloat
        
        def toDoubleValue: DoubleValue = value.toDouble
        
        def toStringValue: StringValue = value.toString
    }
    
    implicit class S2v(value: Short) {
        def toValue: ShortValue = value
        
        def toByteValue: ByteValue = value.toByte
        
        def toShortValue: ShortValue = value.toShort
        
        def toIntValue: IntValue = value.toInt
        
        def toLongValue: LongValue = value.toLong
        
        def toFloatValue: FloatValue = value.toFloat
        
        def toDoubleValue: DoubleValue = value.toDouble
        
        def toStringValue: StringValue = value.toString
    }
    
    implicit class I2v(value: Int) {
        def toValue: IntValue = value
        
        def toByteValue: ByteValue = value.toByte
        
        def toShortValue: ShortValue = value.toShort
        
        def toIntValue: IntValue = value.toInt
        
        def toLongValue: LongValue = value.toLong
        
        def toFloatValue: FloatValue = value.toFloat
        
        def toDoubleValue: DoubleValue = value.toDouble
        
        def toStringValue: StringValue = value.toString
    }
    
    implicit class L2v(value: Long) {
        def toValue: LongValue = value
        
        def toByteValue: ByteValue = value.toByte
        
        def toShortValue: ShortValue = value.toShort
        
        def toIntValue: IntValue = value.toInt
        
        def toLongValue: LongValue = value.toLong
        
        def toFloatValue: FloatValue = value.toFloat
        
        def toDoubleValue: DoubleValue = value.toDouble
        
        def toStringValue: StringValue = value.toString
    }
    
    implicit class F2v(value: Float) {
        def toValue: FloatValue = value
        
        def toByteValue: ByteValue = value.toByte
        
        def toShortValue: ShortValue = value.toShort
        
        def toIntValue: IntValue = value.toInt
        
        def toLongValue: LongValue = value.toLong
        
        def toFloatValue: FloatValue = value.toFloat
        
        def toDoubleValue: DoubleValue = value.toDouble
        
        def toStringValue: StringValue = value.toString
    }
    
    implicit class D2v(value: Double) {
        def toValue: DoubleValue = value
        
        def toByteValue: ByteValue = value.toByte
        
        def toShortValue: ShortValue = value.toShort
        
        def toIntValue: IntValue = value.toInt
        
        def toLongValue: LongValue = value.toLong
        
        def toFloatValue: FloatValue = value.toFloat
        
        def toDoubleValue: DoubleValue = value.toDouble
        
        def toStringValue: StringValue = value.toString
    }
    
    implicit class Bool2v(value: Boolean) {
        def toValue: BooleanValue = value
        
        def toBooleanValue: BooleanValue = value
        
        def toStringValue: StringValue = value.toString
    }
    
    implicit class Char2v(value: Char) {
        def toValue: CharValue = value
        
        def toCharValue: CharValue = value
        
        def toStringValue: StringValue = value.toString
    }
    
    implicit class String2v(value: String) {
        def toValue: StringValue = value
        
        def toStringValue: StringValue = value
    }
    
    implicit def num2b(value: NumericValue[_]): ByteValue = NumericToByte(value)
    
    implicit def num2s(value: NumericValue[_]): ShortValue = NumericToShort(value)
    
    implicit def num2i(value: NumericValue[_]): IntValue = NumericToInt(value)
    
    implicit def num2l(value: NumericValue[_]): LongValue = NumericToLong(value)
    
    implicit def num2f(value: NumericValue[_]): FloatValue = NumericToFloat(value)
    
    implicit def num2d(value: NumericValue[_]): DoubleValue = NumericToDouble(value)
}

