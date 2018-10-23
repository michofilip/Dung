package level

import entity.{Entity, EntityPrinter}
import event.Event

class Level(val entityMap: Map[Int, Entity], val events: Seq[Event]) {
    override def toString: String = EntityPrinter.print(this) //TODO remove this later
}