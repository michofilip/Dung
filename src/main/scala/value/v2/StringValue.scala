package value.v2

import entity.Entity
import value.v2.StringValue._

import scala.language.implicitConversions

private[value] abstract class StringValue extends Value[String] {
    final def +(that: StringValue): StringValue = Concatenate(this, that)
    
    final def length: IntValue = Length(this)
}

object StringValue {
    implicit def num2str(value: NumericValue[_]): StringValue = NumericToString(value)
    
    implicit def str2V(value: String): StringValue = StringConstant(value)
    
    implicit class String2V(value: String) {
        def toValue: StringValue = value
        
        def toStringValue: StringValue = value
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
    
    final case class NumericToString(value: NumericValue[_]) extends StringValue {
        override def getValue(implicit entityMap: Map[Int, Entity]): Option[String] = {
            value.getValue match {
                case Some(v: Byte) => Some(v.toString)
                case Some(v: Short) => Some(v.toString)
                case Some(v: Int) => Some(v.toString)
                case Some(v: Long) => Some(v.toString)
                case Some(v: Float) => Some(v.toString)
                case Some(v: Double) => Some(v.toString)
                case _ => None
            }
        }
    }
    
}