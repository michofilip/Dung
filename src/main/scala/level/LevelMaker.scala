package level

import entity.Entity

object LevelMaker {
    def entitiesToMap(entities: Seq[Entity]): Map[Int, Entity] = {
        def e2m(entities: Seq[Entity], map: Map[Int, Entity]): Map[Int, Entity] = {
            entities match {
                case entity +: rest =>
                    entity match {
                        case _: Entity.NoEntity => e2m(rest, map)
                        case _ => e2m(rest, map + (entity.id -> entity))
                    }
                case Nil => map
            }
        }
        
        e2m(entities, Map.empty)
    }
}
