package entity

import level.Level

object EntityPrinter {
    def print(level: Level): String = {
        "Level:\n" +
                level.entityMap.values.map(EntityPrinter.print).mkString("Entities\n", "\n", "\n") +
                level.events.mkString("Events\n", "\n", "\n")
    }
    
    def print(entity: Entity): String = {
        entity match {
            case en: Entity.NoEntity =>
                val id = en.id
                s"NoEntity(id=$id)"
            case en: Entity.Controller =>
                val id = en.id
                val time = en.time
                val turn = en.turn
                s"Controller(id=$id, time=$time, turn=$turn)"
            case en: Entity.ScriptContainer =>
                val id = en.id
                s"ScriptHolder(id=$id)"
            case en: Entity.ValueContainer =>
                val id = en.id
                val value = en.value
                s"ValueContainer(id=$id, value=$value)"
            case en: Entity.StaticEntity =>
                val id = en.id
                val x = en.position.x
                val y = en.position.y
                val solid = en.physics.solid
                val opaque = en.physics.opaque
                val state = en.state
                val stateChangeTime = en.stateChangeTime
                s"StaticEntity(id=$id, x=$x, y=$y, solid=$solid, opaque=$opaque, state=$state, stateChangeTime=$stateChangeTime)"
            case en: Entity.Switch =>
                val id = en.id
                val x = en.position.x
                val y = en.position.y
                val solid = en.physics.solid
                val opaque = en.physics.opaque
                val state = en.state
                val stateChangeTime = en.stateChangeTime
                s"Switch(id=$id, x=$x, y=$y, solid=$solid, opaque=$opaque, state=$state, stateChangeTime=$stateChangeTime)"
            case en: Entity.Door =>
                val id = en.id
                val x = en.position.x
                val y = en.position.y
                val solid = en.physics.solid
                val opaque = en.physics.opaque
                val state = en.state
                val stateChangeTime = en.stateChangeTime
                val lockCode = en.lockCode
                s"Door(id=$id, x=$x, y=$y, solid=$solid, opaque=$opaque, state=$state, stateChangeTime=$stateChangeTime, lockCode=$lockCode)"
            case _ => "Unknown entity"
        }
    }
}
