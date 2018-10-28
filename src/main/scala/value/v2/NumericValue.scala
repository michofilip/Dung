package value.v2

import scala.language.implicitConversions

private[value] trait NumericValue[T] extends Value[T]

//object NumericValue {
////    implicit def num2b(value: NumericValue[_]): ByteValue = NumericToByte(value)
//
////    implicit def num2s(value: NumericValue[_]): ShortValue = NumericToShort(value)
//
////    implicit def num2i(value: NumericValue[_]): IntValue = NumericToInt(value)
//
////    implicit def num2l(value: NumericValue[_]): LongValue = NumericToLong(value)
//
////    implicit def num2f(value: NumericValue[_]): FloatValue = NumericToFloat(value)
//
////    implicit def num2d(value: NumericValue[_]): DoubleValue = NumericToDouble(value)
//
////    implicit def num2str(value: NumericValue[_]): StringValue = NumericToString(value)
//
//}