package entity

import event.Event
import program.Program
import states.{Direction, State}
import value.Value

sealed abstract class Entity {
    val id: Int
    
    override def toString: String = EntityPrinter.print(this) //TODO remove later
}

object Entity {
    
    // helper classes
    class Position(val x: Int, val y: Int)
    
    class Physics(val solid: Boolean, val opaque: Boolean)
    
    class Frame(val frameId: Int, offsetX: Int, offsetY: Int)
    
    class Animation(private val frames: IndexedSeq[Frame], private val fps: Int, private val isLooped: Boolean) {
        def getFrame(time: Int): Frame = {
            val frameNo = (fps * time) / 1000
            if (isLooped) {
                frames(frameNo % frames.length)
            } else {
                frames(Math.min(frameNo, frames.length - 1))
            }
        }
    }
    
    // basic functionality
    sealed trait MultiState[T <: Entity] extends Entity {
        val state: State
        val stateChangeTime: Long
        val stateChangeBehavior: State => Seq[Event]
        
        def setState(state: State, stateTimeStamp: Long): T
    }
    
    sealed trait MapEntity[T <: Entity] extends Entity {
        val position: Position
        
        def setPosition(position: Position): T
    }
    
    sealed trait Directional[T <: Entity] extends Entity {
        val direction: Direction
        
        def setDirection(direction: Direction): T
    }
    
    sealed trait Physical[T <: Entity] extends Entity with MultiState[T] {
        protected val physicsSelector: State => Physics
        
        def physics: Physics = physicsSelector(state)
    }
    
    sealed trait Animated[T <: Entity] extends Entity with MultiState[T] with Directional[T] {
        protected val animationSelector: (State, Direction) => Animation
        
        def getFrame(time: Long): Frame = animationSelector(state, direction).getFrame((time - stateChangeTime).toInt)
    }
    
    sealed trait General[T <: Entity] extends Entity with MultiState[T] with MapEntity[T] with Directional[T] with Physical[T] with Animated[T]
    
    sealed trait Switchable[T <: Entity] extends General[T]
    
    sealed trait Openable[T <: Entity] extends General[T] {
        val lockCode: Int
    }
    
    sealed trait Character[T <: Entity] extends General[T]
    
    // special functionality
    sealed trait TimeCounter[T <: Entity] extends Entity {
        val time: Long
        
        def setTime(time: Long): T
    }
    
    sealed trait TurnCounter[T <: Entity] extends Entity {
        val turn: Int
        
        def setTurn(turn: Int): T
    }
    
    sealed trait Scripted[T <: Entity] extends Entity {
        protected val programs: IndexedSeq[Program]
        
        def numberOfPrograms: Int = programs.length
        
        def getProgram(programNo: Int): Program = programs(programNo)
    }
    
    sealed trait ValueHolder[T <: Entity] extends Entity {
        val value: Value[_]
        
        def setValue(value: Value[_]): T
    }
    
    // non-map entities
    final class NoEntity(_id: Int) extends Entity {
        override val id: Int = _id
    }
    
    final class Controller(_id: Int, _time: Long, _turn: Int) extends TimeCounter[Controller] with TurnCounter[Controller] {
        override val id: Int = _id
        override val time: Long = _time
        override val turn: Int = _turn
        
        private def copy(time: Long = time, turn: Int = turn): Controller = {
            new Controller(id, time, turn)
        }
        
        def setTime(time: Long): Controller = copy(time = time)
        
        def setTurn(turn: Int): Controller = copy(turn = turn)
    }
    
    final class ScriptContainer(_id: Int, _programs: IndexedSeq[Program]) extends Scripted[ScriptContainer] {
        override val id: Int = _id
        override protected val programs: IndexedSeq[Program] = _programs
    }
    
    final class ValueContainer(_id: Int, _value: Value[_]) extends ValueHolder[ValueContainer] {
        override val id: Int = _id
        override val value: Value[_] = _value
        
        override def setValue(value: Value[_]): ValueContainer = new ValueContainer(id, value)
    }
    
    // map entities
    final class StaticEntity(_id: Int, _position: Position, _direction: Direction, _physics: Physics, _animationSelector: (State, Direction) => Animation) extends General[StaticEntity] {
        override val id: Int = _id
        override val position: Position = _position
        override val direction: Direction = _direction
        override val state: State = State.Default
        override val stateChangeTime: Long = 0
        override val stateChangeBehavior: State => Seq[Event] = _ => Seq.empty
        override protected val physicsSelector: State => Physics = _ => _physics
        override protected val animationSelector: (State, Direction) => Animation = _animationSelector
        
        private def copy(position: Position = position, direction: Direction = direction): StaticEntity =
            new StaticEntity(id, position, direction, physicsSelector(state), animationSelector)
        
        override def setPosition(position: Position): StaticEntity = copy(position = position)
        
        override def setDirection(direction: Direction): StaticEntity = copy(direction = direction)
        
        override def setState(state: State, stateChangeTime: Long): StaticEntity = copy()
    }
    
    final class Switch(_id: Int, _position: Position, _direction: Direction, _state: State, _stateChangeTime: Long, _physicsSelector: State => Physics, _animationSelector: (State, Direction) => Animation) extends Switchable[Switch] {
        override val id: Int = _id
        override val position: Position = _position
        override val direction: Direction = _direction
        override val state: State = if (Switch.acceptedStates.contains(_state)) _state else state
        override val stateChangeTime: Long = _stateChangeTime
        override val stateChangeBehavior: State => Seq[Event] = Switch.behavior(id, state, 1000, 1000)
        override protected val physicsSelector: State => Physics = _physicsSelector
        override protected val animationSelector: (State, Direction) => Animation = _animationSelector
        
        private def copy(position: Position = position, direction: Direction = direction, state: State = state, stateChangeTime: Long = stateChangeTime): Switch =
            new Switch(id, position, direction, state, stateChangeTime, physicsSelector, animationSelector)
        
        override def setPosition(position: Position): Switch = copy(position = position)
        
        override def setDirection(direction: Direction): Switch = copy(direction = direction)
        
        override def setState(state: State, stateChangeTime: Long): Switch = copy(state = state, stateChangeTime = stateChangeTime)
    }
    
    object Switch {
        val acceptedStates: Set[State] = Set(State.OFF, State.SwitchingOFF, State.SwitchingON, State.ON)
        
        def behavior(id: Int, state: State, switchingOFF: Int, switchingON: Int): State => Seq[Event] = {
            case State.OFF => Seq.empty
            case State.SwitchingOFF => Seq(Event.DelayMillis(id, switchingOFF, Event.SetState(id, State.OFF)))
            case State.SwitchingON => Seq(Event.DelayMillis(id, switchingON, Event.SetState(id, State.ON)))
            case State.ON => Seq.empty
            case _ => Seq.empty
        }
    }
    
    final class Door(_id: Int, _position: Position, _direction: Direction, _state: State, _stateChangeTime: Long, _lockCode: Int, _physicsSelector: State => Physics, _animationSelector: (State, Direction) => Animation) extends Openable[Door] {
        override val id: Int = _id
        override val position: Position = _position
        override val direction: Direction = _direction
        override val state: State = if (Door.acceptedStates.contains(_state)) _state else state
        override val stateChangeTime: Long = _stateChangeTime
        override val stateChangeBehavior: State => Seq[Event] = Door.behavior(id, state, 1000, 1000, 1000, 1000)
        override val lockCode: Int = _lockCode
        override protected val physicsSelector: State => Physics = _physicsSelector
        override protected val animationSelector: (State, Direction) => Animation = _animationSelector
        
        private def copy(position: Position = position, direction: Direction = direction, state: State = state, stateChangeTime: Long = stateChangeTime): Door = {
            new Door(id, position, direction, state, stateChangeTime, lockCode, physicsSelector, animationSelector)
        }
        
        override def setPosition(position: Position): Door = copy(position = position)
        
        override def setDirection(direction: Direction): Door = copy(direction = direction)
        
        override def setState(state: State, stateChangeTime: Long): Door = copy(state = state, stateChangeTime = stateChangeTime)
    }
    
    object Door {
        val acceptedStates: Set[State] = Set(State.Open, State.Opening, State.Closing, State.Close, State.Unlocking, State.Locking, State.Locked)
        
        def behavior(id: Int, state: State, opening: Int, closing: Int, unlocking: Int, locking: Int): State => Seq[Event] = {
            case State.Open => Seq.empty
            case State.Opening => Seq(Event.DelayMillis(id, opening, Event.SetState(id, State.Open)))
            case State.Closing => Seq(Event.DelayMillis(id, closing, Event.SetState(id, State.Close)))
            case State.Close => Seq.empty
            case State.Unlocking => Seq(Event.DelayMillis(id, unlocking, Event.SetState(id, State.Close)))
            case State.Locking => Seq(Event.DelayMillis(id, locking, Event.SetState(id, State.Locked)))
            case State.Locked => Seq.empty
            case _ => Seq.empty
        }
    }
    
    final class Player(_id: Int, _position: Position, _direction: Direction, _state: State, _stateChangeTime: Long, _physicsSelector: State => Physics, _animationSelector: (State, Direction) => Animation) extends Character[Player] {
        override val id: Int = _id
        override val position: Position = _position
        override val direction: Direction = _direction
        override val state: State = if (Player.acceptedStates.contains(_state)) _state else state
        override val stateChangeTime: Long = _stateChangeTime
        override val stateChangeBehavior: State => Seq[Event] = Player.behavior(id, state, 1000, 1000)
        override protected val physicsSelector: State => Physics = _physicsSelector
        override protected val animationSelector: (State, Direction) => Animation = _animationSelector
        
        private def copy(position: Position = position, direction: Direction = direction, state: State = state,
                         stateChangeTime: Long = stateChangeTime): Player =
            new Player(id, position, direction, state, stateChangeTime, physicsSelector, animationSelector)
        
        override def setPosition(position: Position): Player = copy(position = position)
        
        override def setDirection(direction: Direction): Player = copy(direction = direction)
        
        override def setState(state: State, stateChangeTime: Long): Player = copy(state = state, stateChangeTime = stateChangeTime)
    }
    
    object Player {
        val acceptedStates: Set[State] = Set(State.Standing, State.Moving)
        
        def behavior(id: Int, state: State, switchingOFF: Int, switchingON: Int): State => Seq[Event] = _ => Seq.empty
    }
    
}