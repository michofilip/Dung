package level

import clock.Clock
import entity.Entity
import event.Event

object LevelProcessor {
    def next(level: Level, externalEvents: Seq[Event])(implicit clock: Clock): Level = {
        implicit val entityMap: Map[Int, Entity] = level.entityMap
        val eventMap = (Event.SetTime(0, clock.getTime) +: level.events).groupBy(_.id).withDefaultValue(Seq.empty)
        
        def applyTo(entity: Entity, events: Seq[Event]): (Entity, Seq[Event], Seq[Entity]) = {
            def ev2en(entity: Entity, events: Seq[Event], returnEvents: Seq[Event], returnEntities: Seq[Entity]): (Entity, Seq[Event], Seq[Entity]) = {
                events match {
                    case event +: rest =>
                        val (newEntity, newEvents, newEntities) = event.applyTo(entity)
                        ev2en(newEntity, rest, newEvents ++ returnEvents, newEntities ++ returnEntities)
                    case Nil => (entity, returnEvents, returnEntities)
                }
            }
            
            ev2en(entity, events, Seq.empty, Seq.empty)
        }
        
        def actualizeEntities(entities: Seq[Entity]): (Seq[Entity], Seq[Event]) = {
            def act(entities: Seq[Entity], newEntities: Seq[Entity], returnEvents: Seq[Event]): (Seq[Entity], Seq[Event]) = {
                entities match {
                    case entity +: rest =>
                        val (newEntity, newEvents, returnEntities) = applyTo(entity, eventMap(entity.id))
                        act(rest, (newEntity +: returnEntities) ++ newEntities, newEvents ++ returnEvents)
                    case Nil => (newEntities, returnEvents)
                }
            }
            
            act(entities, Seq.empty, externalEvents)
        }
        
        val (newEntities, returnEvents) = actualizeEntities(entityMap.values.toSeq)
        new Level(LevelMaker.entitiesToMap(newEntities), returnEvents)
    }
}
