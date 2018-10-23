package program

import entity.Entity
import event.Event._
import program.Instruction._
import program.Statement._
import states.State
import value.CustomValue._
import value.Value
import value.ValueImplicits._

import scala.language.implicitConversions

sealed abstract class Program() {
    protected val instructions: IndexedSeq[Instruction]
    protected lazy val labelMap: Map[Int, Int] = Compiler.mapLabels(instructions)
    
    def nextLine(line: Int): Int = {
        def nl(line: Int): Int = {
            getInstruction(line) match {
                case EX => line
                case DO(_) => line
                case LB(_) => nl(line + 1)
                case GT(labelId) => getLabelLineNo(labelId) match {
                    case Some(l) => nl(l + 1)
                    case None => nl(line + 1)
                }
                case IF(_) => line
            }
        }
        
        nl(line)
    }
    
    def getInstruction(lineId: Int): Instruction = {
        if (0 <= lineId && lineId < instructions.length) {
            instructions(lineId)
        } else {
            EX
        }
    }
    
    def getLabelLineNo(labelId: Int): Option[Int] = labelMap.get(labelId)
    
    //  override def toString: String = s"Program(${instructions.mkString(", ")})"
    
    def toPrettyString: String = {
        val len = (instructions.length - 1).toString.length
        val instStr = instructions.zipWithIndex.map {
            case (inst, line) =>
                val lineStr = Seq.fill(len - line.toString.length)(" ").mkString + line
                s"$lineStr|  $inst"
        }.mkString("\n")
        val labels = labelMap.toList.sortWith {
            case ((id1, _), (id2, _)) => id1 < id2
        }.map {
            case (id, line) => s"$id -> $line"
        }.mkString("\n")
        
        s"Program:\n$labels\n$instStr"
    }
}

object Program {
    
    case object EmptyProgram extends Program() {
        override protected val instructions: IndexedSeq[Instruction] = IndexedSeq(EX)
    }
    
    case class CustomProgram(statement: Statement) extends Program() {
        override protected val instructions: IndexedSeq[Instruction] = Compiler.compile(statement)
    }
    
    case class Collatz(x: Int) extends Program() {
        override protected val instructions: IndexedSeq[Instruction] = Compiler.compile(
            block(
                Seq(
                    Spawn(0, new Entity.ValueContainer(1000, Value(x))),
                    Spawn(0, new Entity.ValueContainer(1001, Value(1)))
                ),
                loop(ExistEntity(1000) !&& ExistEntity(1001))(),
                loop(GetInt(1000) =!= Value(1))(
                    when(GetInt(1000) % Value(2) === Value(0))(
                        SetCalculatedValue(1000, GetInt(1000) / Value(2))
                    )(
                        SetCalculatedValue(1000, GetInt(1000) * Value(3) + Value(1))
                    ),
                    SetCalculatedValue(1001, GetInt(1001) + Value(1))
                ),
                Kill(1000)
            )
        )
    }
    
    case class DoorBehavior(mapTimerId: Int, entityId: Int, openingTime: Int, closingTime: Int, unlockingTime: Int, lockingTime: Int) extends Program() {
        override protected val instructions: IndexedSeq[Instruction] = Compiler.compile(
            switch(GetState(1000))(
                variant(State.Opening)(
                    loop(GetMapTime(mapTimerId) - GetStateChangeTime(entityId) < openingTime.toLongValue)(),
                    SetState(entityId, State.Open)
                ),
                variant(State.Closing)(
                    loop(GetMapTime(mapTimerId) - GetStateChangeTime(entityId) < closingTime.toLongValue)(),
                    SetState(entityId, State.Close)
                ),
                variant(State.Unlocking)(
                    loop(GetMapTime(mapTimerId) - GetStateChangeTime(entityId) < unlockingTime.toLongValue)(),
                    SetState(entityId, State.Close)
                ),
                variant(State.Locking)(
                    loop(GetMapTime(mapTimerId) - GetStateChangeTime(entityId) < lockingTime.toLongValue)(),
                    SetState(entityId, State.Locked)
                ),
            )()
        )
    }
    
    case class SwitchBehavior(mapTimerId: Int, entityId: Int, switchingOff: Int, switchingOn: Int) extends Program() {
        override protected val instructions: IndexedSeq[Instruction] = Compiler.compile(
            switch(GetState(1000))(
                variant(State.SwitchingOFF)(
                    loop(GetMapTime(mapTimerId) - GetStateChangeTime(entityId) < switchingOff.toLongValue)(),
                    SetState(entityId, State.Open)
                ),
                variant(State.SwitchingON)(
                    loop(GetMapTime(mapTimerId) - GetStateChangeTime(entityId) < switchingOn.toLongValue)(),
                    SetState(entityId, State.Close)
                ),
            )()
        )
    }
    
    //  def behavior(id: Int, state: State, opening: Int, closing: Int, unlocking: Int, locking: Int)
}
