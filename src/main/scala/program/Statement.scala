package program

import event.Event
import value.Value
import value.Value.BooleanValue

import scala.language.implicitConversions

sealed abstract class Statement

object Statement {
    implicit def ev2s(event: Event): Do = Do(event)
    
    implicit def evs2s(events: Seq[Event]): Do = Do(events)
    
    def block(statements: Statement*): Block = {
        Block(statements)
    }
    
    def when(condition: BooleanValue)(thenStatements: Statement*)(elseStatements: Statement*): When = {
        When(condition, Block(thenStatements), Block(elseStatements))
    }
    
    def loop(condition: BooleanValue)(loopedStatements: Statement*): Loop = {
        Loop(condition, Block(loopedStatements))
    }
    
    def variant(variantTest: Value[_])(variantStatements: Statement*): Variant = {
        Variant(variantTest, Block(variantStatements))
    }
    
    def switch(switchTest: Value[_])(variants: Variant*)(defaultStatements: Statement*): Switch = {
        Switch(switchTest, variants, Block(defaultStatements))
    }
    
    case class Do(events: Seq[Event]) extends Statement
    
    case class Block(statements: Seq[Statement]) extends Statement
    
    case class When(condition: BooleanValue, thenStatement: Statement, elseStatement: Statement) extends Statement
    
    case class Loop(condition: BooleanValue, loopedStatement: Statement) extends Statement
    
    case class Variant(variantTest: Value[_], variantStatement: Statement)
    
    case class Switch(switchTest: Value[_], variants: Seq[Variant], defaultStatement: Statement) extends Statement
    
}