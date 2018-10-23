package states

sealed abstract class Direction

object Direction {
    
    case object North extends Direction
    
    case object NorthEast extends Direction
    
    case object East extends Direction
    
    case object SouthEast extends Direction
    
    case object South extends Direction
    
    case object SouthWest extends Direction
    
    case object West extends Direction
    
    case object NorthWest extends Direction
    
}