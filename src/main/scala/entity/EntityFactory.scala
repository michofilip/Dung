package entity

import entity.Entity._
import states.{Direction, State}

sealed abstract class EntityFactory

object EntityFactory {
    
    case object Floor extends EntityFactory {
        def make(id: Int, x: Int, y: Int): StaticEntity = {
            new StaticEntity(id: Int, new Position(x, y), Direction.North, new Physics(solid = false, opaque = false), (_, _) => new Animation(IndexedSeq(new Frame(0, 0, 0)), 25, false))
        }
    }
    
    case object Wall extends EntityFactory {
        def make(id: Int, x: Int, y: Int): StaticEntity = {
            new StaticEntity(id: Int, new Position(x, y), Direction.North,
                new Physics(solid = true, opaque = true),
                (_, _) => new Animation(IndexedSeq(new Frame(0, 0, 0)), 25, false))
        }
    }
    
    private val switchPhysics: State => Physics = _ => new Physics(true, false)
    private val switchGraphics: (State, Direction) => Animation = (_, _) => new Animation(IndexedSeq(new Frame(0, 0, 0)), 25, false)
    
    private val opaqueDoorPhysics: State => Physics = {
        case State.Open => new Physics(false, false)
        case State.Opening => new Physics(true, false)
        case State.Closing => new Physics(true, false)
        case State.Close => new Physics(true, true)
        case State.Unlocking => new Physics(true, true)
        case State.Locking => new Physics(true, true)
        case State.Locked => new Physics(true, true)
        case _ => new Physics(false, false)
    }
    private val opaqueDoorGraphics: (State, Direction) => Animation = (_, _) => new Animation(IndexedSeq(new Frame(0, 0, 0)), 25, false)
    
    private val transparentDoorPhysics: State => Physics = {
        case State.Open => new Physics(false, false)
        case State.Opening => new Physics(true, false)
        case State.Closing => new Physics(true, false)
        case State.Close => new Physics(true, false)
        case State.Unlocking => new Physics(true, false)
        case State.Locking => new Physics(true, false)
        case State.Locked => new Physics(true, false)
        case _ => new Physics(false, false)
    }
    private val transparentDoorGraphics: (State, Direction) => Animation = (_, _) => new Animation(IndexedSeq(new Frame(0, 0, 0)), 25, false)
    
    def floor(id: Int, x: Int, y: Int): StaticEntity = {
        new StaticEntity(id: Int, new Position(x, y), Direction.North, new Physics(solid = false, opaque = false), (_, _) => new Animation(IndexedSeq(new Frame(0, 0, 0)), 25, false))
    }
    
    def wall(id: Int, x: Int, y: Int): StaticEntity = {
        new StaticEntity(id: Int, new Position(x, y), Direction.North, new Physics(solid = true, opaque = true), (_, _) => new Animation(IndexedSeq(new Frame(0, 0, 0)), 25, false))
    }
    
    def switch(id: Int, x: Int, y: Int, state: State): Switch = {
        val _state = if (Switch.acceptedStates.contains(state)) state else State.OFF
        new Switch(id: Int, new Position(x, y), Direction.North, _state, 0, switchPhysics, switchGraphics)
    }
    
    def opaqueDoor(id: Int, x: Int, y: Int, state: State, lockCode: Int): Door = {
        val _state = if (Door.acceptedStates.contains(state)) state else State.Open
        new Door(id: Int, new Position(x, y), Direction.North, _state, 0, lockCode, opaqueDoorPhysics, opaqueDoorGraphics)
    }
    
    def transparentDoor(id: Int, x: Int, y: Int, state: State, lockCode: Int): Door = {
        val _state = if (Door.acceptedStates.contains(state)) state else State.Open
        new Door(id: Int, new Position(x, y), Direction.North, _state, 0, lockCode, transparentDoorPhysics, transparentDoorGraphics)
    }
}
