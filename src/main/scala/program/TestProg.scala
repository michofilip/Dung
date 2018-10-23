package program

// TODO test class, remove later
object TestProg extends App {
    val prog1 = Program.Collatz(100)
    //  val prog2 = Program.DoorBehavior(0, 10, 1000, 1000, 1000, 1000)
    //  val prog3 = Program.SwitchBehavior(0, 10, 1000, 1000)
    
    println(prog1.toPrettyString)
    println()
    //  println(prog2.toPrettyString)
    //  println()
    //  println(prog3.toPrettyString)
    //  println()
}
