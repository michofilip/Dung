package clock

class Clock(private val start: Long) {
    private val time0: Long = System.currentTimeMillis()
    
    def getTime: Long = {
        System.currentTimeMillis() - time0 + start
    }
}
