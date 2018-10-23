import clock.Clock
import entity.Entity
import event.Event
import level.{Level, LevelMaker, LevelProcessor}
import program.Program

object Main2 extends App {
    implicit val clock: Clock = new Clock(0)
    val entities = Seq(
        new Entity.Controller(0, clock.getTime, 0),
        new Entity.ScriptContainer(10, IndexedSeq(Program.Collatz(100)))
    )
    val events: Seq[Event] = Seq(
        Event.ExecuteScript(10, 0)
    )
    
    var level = new Level(LevelMaker.entitiesToMap(entities), events)
    println(level)
    //  Thread.sleep(500)
    
    for (i <- 0 until 200) {
        level = LevelProcessor.next(level, Seq.empty)
        println(i)
        println(level)
    }
    
    
    //  val clock2 = new Clock(0)
    //  val start = clock2.getTime
    //  while (clock2.getTime - start <= 3000) {
    //    Thread.sleep(1)
    //    if ((clock2.getTime - start) % 100 == 0) {
    //      println(level)
    //    }
    //    level = LevelProcessor.next(level, Seq.empty)
    //  }
    //  println(level)
}
