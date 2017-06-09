package thesis.utils

case class Direction(row:Int, col:Int){
	def opposite = Direction(-row, -col)
	override def toString = this match {
		case Direction.UP => "Up"
		case Direction.DOWN => "Down"
		case Direction.LEFT => "Left"
		case Direction.RIGHT => "Right"
	}
}
object Direction {
  val UP 	= 	Direction(-1, 0)
  val DOWN 	=	Direction(1, 0)
  val RIGHT = 	Direction(0, 1)
  val LEFT 	= 	Direction(0,-1)
}