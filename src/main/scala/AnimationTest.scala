import clock.Clock
import entity.Entity._
import states.{Direction, State}

object AnimationTest extends App {
    val clock = new Clock(0)
    val frames = IndexedSeq.tabulate(4)(i => new Frame(i, 0, 0))
    val as: (State, Direction) => Animation = (_, _) => new Animation(frames, 30, true)
    val ent1 = new StaticEntity(0, new Position(10, 20), Direction.North, new Physics(true, true), as)
    val images = IndexedSeq('-', '\\', '|', '/')
    
    var start = clock.getTime
    while (clock.getTime - start <= 1000) {
        Thread.sleep(1)
        println(images(ent1.getFrame(clock.getTime).frameId))
        //    if ((clock.getTime - start) % 100 == 0) {
        //      println(ent1.getFrame(clock.getTime).frameId)
        //    }
    }
}
