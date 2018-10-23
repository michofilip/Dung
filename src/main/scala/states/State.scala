package states

abstract sealed class State

object State {
    
    case object Default extends State
    
    // switch
    case object OFF extends State
    
    case object SwitchingOFF extends State
    
    case object SwitchingON extends State
    
    case object ON extends State
    
    // door
    case object Open extends State
    
    case object Opening extends State
    
    case object Closing extends State
    
    case object Close extends State
    
    case object Unlocking extends State
    
    case object Locking extends State
    
    case object Locked extends State
    
    // character
    case object Standing extends State
    
    case object Moving extends State
    
    
}