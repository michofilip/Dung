package event

import entity.Entity
import entity.Entity._
import event.Event.{Chain, DelayMillis, DelayTurns, ScheduleMillis, ScheduleTurns}
import program.Instruction.{DO, EX, IF}
import program.Program
import states.{Direction, State}
import value.Value
import value.Value.IntValue
import value.ValueImplicits._
import value.CustomValue._

import scala.language.implicitConversions

sealed abstract class Event() {
  val id: Int

  def applyTo(entity: Entity)(implicit entityMap: Map[Int, Entity]): (Entity, Seq[Event], Seq[Entity])

  def andThen(that: Event): Chain = (this, that) match {
    case (Chain(_, thisEventsSeq), Chain(_, thatEventsSeq)) => Chain(id, thisEventsSeq ++ thatEventsSeq)
    case (_, Chain(_, eventsSeq)) => Chain(id, Seq(this) +: eventsSeq)
    case (Chain(_, eventsSeq), _) => Chain(id, eventsSeq :+ Seq(that))
    case _ => Chain(id, Seq(this, that))
  }

  def delayMillis(delay: Int): Event = this match {
    case ScheduleMillis(_, time, evs) => ScheduleMillis(id, time + delay, evs)
    case DelayMillis(_, thisDelay, evs) => DelayMillis(id, thisDelay + delay, evs)
    case _ => DelayMillis(id, delay, this)
  }

  def delayTurns(delay: Int): Event = this match {
    case ScheduleTurns(_, time, evs) => ScheduleTurns(id, time + delay, evs)
    case DelayTurns(_, thisDelay, evs) => DelayTurns(id, thisDelay + delay, evs)
    case _ => DelayTurns(id, delay, this)
  }
}

object Event {

  implicit def ev2seq(event: Event): Seq[Event] = Seq(event)

  implicit def en2seq(entity: Entity): Seq[Entity] = Seq(entity)

  // entity management
  final case class Kill(override val id: Int) extends Event {
    override def applyTo(entity: Entity)(implicit entityMap: Map[Int, Entity]): (Entity, Seq[Event], Seq[Entity]) = {
      (new NoEntity(id), Seq.empty, Seq.empty)
    }
  }

  final case class Spawn(override val id: Int, entities: Seq[Entity]) extends Event {
    override def applyTo(entity: Entity)(implicit entityMap: Map[Int, Entity]): (Entity, Seq[Event], Seq[Entity]) = {
      entity match {
        case _: Controller => (entity, Seq.empty, entities)
        case _ => (entity, Seq.empty, Seq.empty)
      }
    }
  }

  // move
  final case class MoveTo(override val id: Int, x: Int, y: Int) extends Event {
    override def applyTo(entity: Entity)(implicit entityMap: Map[Int, Entity]): (Entity, Seq[Event], Seq[Entity]) = {
      entity match {
        case ent: MapEntity[_] => (ent.setPosition(new Position(x, y)), Seq.empty, Seq.empty)
        case _ => (entity, Seq.empty, Seq.empty)
      }
    }
  }

  final case class MoveToValue(override val id: Int, x: IntValue, y: IntValue) extends Event {
    override def applyTo(entity: Entity)(implicit entityMap: Map[Int, Entity]): (Entity, Seq[Event], Seq[Entity]) = {
      (entity, x.getValue, y.getValue) match {
        case (en: MapEntity[_], Some(x1), Some(y1)) => (en.setPosition(new Position(x1, y1)), Seq.empty, Seq.empty)
        case _ => (entity, Seq.empty, Seq.empty)
      }
    }
  }

  final case class MoveBy(override val id: Int, dx: Int, dy: Int) extends Event {
    override def applyTo(entity: Entity)(implicit entityMap: Map[Int, Entity]): (Entity, Seq[Event], Seq[Entity]) = {
      entity match {
        case ent: MapEntity[_] =>
          val x = ent.position.x
          val y = ent.position.y
          (ent.setPosition(new Position(x + dx, y + dy)), Seq.empty, Seq.empty)
        case _ => (entity, Seq.empty, Seq.empty)
      }
    }
  }

  // switch
  final case class SwitchOFF(override val id: Int) extends Event {
    override def applyTo(entity: Entity)(implicit entityMap: Map[Int, Entity]): (Entity, Seq[Event], Seq[Entity]) =
      entity match {
        case en: Switchable[_] if en.state == State.ON =>
          (en, SetState(id, State.SwitchingOFF), Seq.empty)
        case _ => (entity, Seq.empty, Seq.empty)
      }
  }

  final case class SwitchON(override val id: Int) extends Event {
    override def applyTo(entity: Entity)(implicit entityMap: Map[Int, Entity]): (Entity, Seq[Event], Seq[Entity]) = {
      entity match {
        case en: Switchable[_] if en.state == State.OFF =>
          (en, SetState(id, State.SwitchingON), Seq.empty)
        case _ => (entity, Seq.empty, Seq.empty)
      }
    }
  }

  // door
  final case class Open(override val id: Int) extends Event {
    override def applyTo(entity: Entity)(implicit entityMap: Map[Int, Entity]): (Entity, Seq[Event], Seq[Entity]) = {
      entity match {
        case en: Openable[_] if en.state == State.Close =>
          (en, SetState(id, State.Opening), Seq.empty)
        case _ => (entity, Seq.empty, Seq.empty)
      }
    }
  }

  final case class Close(override val id: Int) extends Event {
    override def applyTo(entity: Entity)(implicit entityMap: Map[Int, Entity]): (Entity, Seq[Event], Seq[Entity]) = {
      entity match {
        case en: Openable[_] if en.state == State.Open =>
          (en, SetState(id, State.Closing), Seq.empty)
        case _ => (entity, Seq.empty, Seq.empty)
      }
    }
  }

  final case class Unlock(override val id: Int, keyCodes: Set[Int]) extends Event {
    override def applyTo(entity: Entity)(implicit entityMap: Map[Int, Entity]): (Entity, Seq[Event], Seq[Entity]) = {
      entity match {
        case en: Openable[_] if en.state == State.Locked && keyCodes.contains(en.lockCode) =>
          (en, SetState(id, State.Unlocking), Seq.empty)
        case _ => (entity, Seq.empty, Seq.empty)
      }
    }
  }

  final case class Lock(override val id: Int, keyCodes: Set[Int]) extends Event {
    override def applyTo(entity: Entity)(implicit entityMap: Map[Int, Entity]): (Entity, Seq[Event], Seq[Entity]) = {
      entity match {
        case en: Openable[_] if en.state == State.Close && keyCodes.contains(en.lockCode) =>
          (en, SetState(id, State.Locking), Seq.empty)
        case _ => (entity, Seq.empty, Seq.empty)
      }
    }
  }

  // script executors
  final case class ExecuteScript(override val id: Int, programNo: Int) extends Event {
    override def applyTo(entity: Entity)(implicit entityMap: Map[Int, Entity]): (Entity, Seq[Event], Seq[Entity]) = {
      entity match {
        case script: Scripted[_] if 0 <= programNo && programNo < script.numberOfPrograms =>
          (entity, ExecuteProgram(id, script.getProgram(programNo), 0), Seq.empty)
        case _ => (entity, Seq.empty, Seq.empty)
      }
    }
  }

  final case class ExecuteProgram(override val id: Int, program: Program, lineNo: Int) extends Event {
    override def applyTo(entity: Entity)(implicit entityMap: Map[Int, Entity]): (Entity, Seq[Event], Seq[Entity]) = {
      program.getInstruction(lineNo) match {
        case DO(events) => (entity, events ++ ExecuteProgram(id, program, program.nextLine(lineNo + 1)), Seq.empty)
        case IF(condition) => condition.getValue match {
          case Some(true) => (entity, ExecuteProgram(id, program, program.nextLine(lineNo + 2)), Seq.empty)
          case Some(false) => (entity, ExecuteProgram(id, program, program.nextLine(lineNo + 1)), Seq.empty)
          case None => (entity, Seq.empty, Seq.empty)
        }
        case EX => (entity, Seq.empty, Seq.empty)
        case _ => (entity, ExecuteProgram(id, program, program.nextLine(lineNo + 1)), Seq.empty)
      }
    }
  }

  // setters
  final case class SetState(override val id: Int, state: State) extends Event {
    override def applyTo(entity: Entity)(implicit entityMap: Map[Int, Entity]): (Entity, Seq[Event], Seq[Entity]) = {
      (entityMap.get(0), entity) match {
        case (Some(controller: TimeCounter[_]), multi: MultiState[_]) =>
          (multi.setState(state, controller.time), multi.stateChangeBehavior(state), Seq.empty)
        case _ => (entity, Seq.empty, Seq.empty)
      }
    }
  }

  final case class SetValue(override val id: Int, value: Value[_]) extends Event {
    override def applyTo(entity: Entity)(implicit entityMap: Map[Int, Entity]): (Entity, Seq[Event], Seq[Entity]) = {
      entity match {
        case en: ValueHolder[_] => (en.setValue(value), Seq.empty, Seq.empty)
        case _ => (entity, Seq.empty, Seq.empty)
      }
    }
  }

  final case class SetCalculatedValue(override val id: Int, value: Value[_]) extends Event {
    override def applyTo(entity: Entity)(implicit entityMap: Map[Int, Entity]): (Entity, Seq[Event], Seq[Entity]) = {
      entity match {
        case en: ValueHolder[_] => value.getValue match {
          case Some(value: Byte) => (en.setValue(value.toByteValue), Seq.empty, Seq.empty)
          case Some(value: Short) => (en.setValue(value.toShortValue), Seq.empty, Seq.empty)
          case Some(value: Int) => (en.setValue(value.toIntValue), Seq.empty, Seq.empty)
          case Some(value: Long) => (en.setValue(value.toLongValue), Seq.empty, Seq.empty)
          case Some(value: Float) => (en.setValue(value.toFloatValue), Seq.empty, Seq.empty)
          case Some(value: Double) => (en.setValue(value.toDoubleValue), Seq.empty, Seq.empty)
          case Some(value: Boolean) => (en.setValue(value.toBooleanValue), Seq.empty, Seq.empty)
          case Some(value: Char) => (en.setValue(value.toCharValue), Seq.empty, Seq.empty)
          case Some(value: String) => (en.setValue(value.toStringValue), Seq.empty, Seq.empty)
          case Some(value: State) => (en.setValue(value), Seq.empty, Seq.empty)
          case Some(value: Direction) => (en.setValue(value), Seq.empty, Seq.empty)
          case _ => (entity, Seq.empty, Seq.empty)
        }
        case _ => (entity, Seq.empty, Seq.empty)
      }
    }
  }

  final case class SetTime(override val id: Int, time: Long) extends Event {
    override def applyTo(entity: Entity)(implicit entityMap: Map[Int, Entity]): (Entity, Seq[Event], Seq[Entity]) = {
      entity match {
        case controller: TimeCounter[_] => (controller.setTime(time), Seq.empty, Seq.empty)
        case _ => (entity, Seq.empty, Seq.empty)
      }
    }
  }

  final case class SetTurn(override val id: Int, turn: Int) extends Event {
    override def applyTo(entity: Entity)(implicit entityMap: Map[Int, Entity]): (Entity, Seq[Event], Seq[Entity]) = {
      entity match {
        case controller: TurnCounter[_] => (controller.setTurn(turn), Seq.empty, Seq.empty)
        case _ => (entity, Seq.empty, Seq.empty)
      }
    }
  }

  final case class NextTurn(override val id: Int) extends Event {
    override def applyTo(entity: Entity)(implicit entityMap: Map[Int, Entity]): (Entity, Seq[Event], Seq[Entity]) = {
      entity match {
        case controller: TurnCounter[_] => (controller.setTurn(controller.turn + 1), Seq.empty, Seq.empty)
        case _ => (entity, Seq.empty, Seq.empty)
      }
    }
  }

  // event management
  final case class Chain(override val id: Int, eventsSeq: Seq[Seq[Event]]) extends Event {
    override def applyTo(entity: Entity)(implicit entityMap: Map[Int, Entity]): (Entity, Seq[Event], Seq[Entity]) = {
      eventsSeq match {
        case Nil => (entity, Seq.empty, Seq.empty)
        case events +: Nil => (entity, events, Seq.empty)
        case events +: rest => (entity, Chain(id, rest) +: events, Seq.empty)
      }
    }
  }

  final case class DelayMillis(override val id: Int, delay: Int, events: Seq[Event]) extends Event {
    override def applyTo(entity: Entity)(implicit entityMap: Map[Int, Entity]): (Entity, Seq[Event], Seq[Entity]) = {
      entityMap.get(0) match {
        case Some(controller: TimeCounter[_]) =>
          (entity, ScheduleMillis(id, controller.time + delay, events), Seq.empty)
        case _ => (entity, events, Seq.empty)
      }
    }
  }

  final case class ScheduleMillis(override val id: Int, time: Long, events: Seq[Event]) extends Event {
    override def applyTo(entity: Entity)(implicit entityMap: Map[Int, Entity]): (Entity, Seq[Event], Seq[Entity]) = {
      entityMap.get(0) match {
        case Some(controller: TimeCounter[_]) =>
          if (controller.time >= time) {
            (entity, events, Seq.empty)
          } else {
            (entity, this, Seq.empty)
          }
        case _ => (entity, events, Seq.empty)
      }
    }
  }

  final case class DelayTurns(override val id: Int, delay: Int, events: Seq[Event]) extends Event {
    override def applyTo(entity: Entity)(implicit entityMap: Map[Int, Entity]): (Entity, Seq[Event], Seq[Entity]) = {
      entityMap.get(0) match {
        case Some(controller: TurnCounter[_]) =>
          (entity, ScheduleTurns(id, controller.turn + delay, events), Seq.empty)
        case _ => (entity, events, Seq.empty)
      }
    }
  }

  final case class ScheduleTurns(override val id: Int, turn: Int, events: Seq[Event]) extends Event {
    override def applyTo(entity: Entity)(implicit entityMap: Map[Int, Entity]): (Entity, Seq[Event], Seq[Entity]) = {
      entityMap.get(0) match {
        case Some(controller: TurnCounter[_]) =>
          if (controller.turn >= turn) {
            (entity, events, Seq.empty)
          } else {
            (entity, this, Seq.empty)
          }
        case _ => (entity, events, Seq.empty)
      }
    }
  }

}