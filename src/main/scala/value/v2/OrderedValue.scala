package value.v2

import value.v2.BooleanValue.{Grater, GraterEqual, Less, LessEqual}

private[value] trait OrderedValue[T] extends Value[T] {
    def <(that: OrderedValue[_]): Less = Less(this, that)
    
    def <=(that: OrderedValue[_]): LessEqual = LessEqual(this, that)
    
    def >(that: OrderedValue[_]): Grater = Grater(this, that)
    
    def >=(that: OrderedValue[_]): GraterEqual = GraterEqual(this, that)
}
