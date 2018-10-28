package value.v2

import entity.Entity
import value.v2.BooleanValue.{BooleanConstant, Equals, Unequals}
import value.v2.ByteValue.ByteConstant
import value.v2.CharValue.CharConstant
import value.v2.DoubleValue.DoubleConstant
import value.v2.FloatValue.FloatConstant
import value.v2.IntValue.IntConstant
import value.v2.LongValue.LongConstant
import value.v2.ShortValue.ShortConstant
import value.v2.StringValue.StringConstant

abstract class Value[T] {
    def getValue(implicit entityMap: Map[Int, Entity]): Option[T]
    
    def ===(that: Value[_]): BooleanValue = Equals(this, that)
    
    def =!=(that: Value[_]): BooleanValue = Unequals(this, that)
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
}