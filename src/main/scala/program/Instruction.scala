package program

import event.Event
import value.Value.BooleanValue

sealed abstract class Instruction

object Instruction {
    
    final case object EX extends Instruction
    
    final case class DO(events: Seq[Event]) extends Instruction
    
    final case class LB(labelId: Int) extends Instruction
    
    final case class GT(labelId: Int) extends Instruction
    
    final case class IF(condition: BooleanValue) extends Instruction
    
}