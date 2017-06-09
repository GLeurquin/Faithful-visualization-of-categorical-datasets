package thesis.orderings
import thesis.matrixTypes._
import thesis.utils._
import thesis.rectangles._

abstract class Move {
	def apply(m: MatrixMoves):ReverseMove
}

/**
 * Object containing the different moves that can be used
 */
object Move {

	/** Swap move
	* 	Selects two random rows a and b such that b - a < 10 and swaps them
	*/
	case object Swap extends Move{
		def apply(m: MatrixMoves):ReverseMove = {
			val a = m.randomRow
			val b = Math.min(m.rows-1, a+Utils.rand.nextInt(10))
			m.swap(a,b)
		}
		override def toString = "Swap"
	}

	/** Swap Adjacent move
	* 	Selects two random adjacent rows and swaps them
	*/
	case object SwapAdjacent extends Move{
		def apply(m: MatrixMoves):ReverseMove = {
			var a = m.randomRow

			if(a == m.rows - 1) a -= 1
			var b = (a+1) % m.rows
			m.swap(a,b)
		}
		override def toString = "SwapAdjacent"
	}

	/** Reverse move
	* 	Selects two random rows a and i such that |a-i| < 50 and reverses all the rows between them (included)
	*/
	case object Reverse extends Move{
		def apply(m: MatrixMoves):ReverseMove = {
			var a = m.randomRow
			var i = if(Utils.rand.nextInt(2) == 0.0) Math.min(a+Utils.rand.nextInt(50), m.rows-1) else Math.max(a-Utils.rand.nextInt(50), 0)
			m.reverse(a,i)
		}
		override def toString = "Reverse"
	}

	/** Reverse move
	* 	Selects two random rows a and b such that |a-i| < 50 and
	* 	a random k shifts the rows a to b (inclusive) forward(if a<b)/backwards(if a>b) by a k rows
	*/
	case object Relocate extends Move{
		def apply(m: MatrixMoves):ReverseMove = {
			val a = m.randomRow
			var b = if(Utils.rand.nextInt(2) == 0.0) Math.min(a+Utils.rand.nextInt(50), m.rows-1) else Math.max(a-Utils.rand.nextInt(50), 0)

			val k = Utils.rand.nextInt{
				if(a < b) m.rows - b
				else b.max(1)
			}

			m.relocate(a,b,k)

		}
		override def toString = "Relocate"
	}

	/**	Kmax move
	* 	Cut the matrix into maximum k+1 blocks separated by k rows for which the error is the
	* 	greatest. Then reorder those blocks with a TSP. Do it until convergence
	*/
	case object KMax extends Move{
		def apply(m: MatrixMoves):ReverseMove = {
			val k = 10
			m.apply_kmax(k)
		}
		override def toString = "KMax"
	}

	/** Swap Rectangles move
	* 	Finds two rectangles in the matrix and put them next to each other
	*/
	case object SwapRectangles extends Move{
		def apply(m: MatrixMoves):ReverseMove = {
			val rectArray = RectangleFinder.getSuitableRectangles(m)
			val r1 = rectArray(0)
			val r2 = rectArray(1)

			var reverseColor = new ReverseMove(){
				def reverseTheMove = Unit
				def moveName = "reverseColor"
			}

			// If the matrix is categorical and that it is transposed,
			// change the color of one rectangle such that both have the same color
			if(m.isCat && m.isTranspose) reverseColor = m.changeColorRectangles(r1, r2)
			val reverse1 = m.swapRectangles(r1, r2)

			new ReverseMove(){
				def reverseTheMove = {
					reverse1.reverseTheMove
					reverseColor.reverseTheMove
				}
				def moveName = "full SwapRectangle"
			}
		}
		override def toString = "Rectangles"
	}
}

/** Class that represents a move with its probability of being selected
*/
case class MoveProba(move:Move, var probaWeight:Double = 1.0){ // Default weight of 1.0 if not provided
	require(probaWeight >= 0, s"The weight must be >= 0 : $probaWeight")

	/** Multiplies the probability weight of this move by weight
	*
	*	@param weight The weight multiplier
	*/
	def *=(weight:Double){
		require(weight >= 0, s"The weight multiplier must be >= 0 : $weight")
		probaWeight *= weight
	}

	/** Adds weight to the probability weight of this move
	*
	*	@param weight The weight to add
	*/
	def +=(weight:Double){
		probaWeight += weight
		probaWeight = probaWeight.max(0.0)
	}

	var contributedError = 0.0 // Keeps track of the amount of error each move contributed for
	var count = 0  // Counts how many times each move was used in the last segment.

	var time:Long = 0

	/** Resets the learning of the probability for this move
	*/
	def reset(){
		contributedError = 0.0
		count = 0
		time = 0
	}

	/** Learn the new probability for this move
	*	
	*	@param reaction_factor If equal to 0, will never update the weight. 
	*	If equal to 0, will never take into account old weight
	*/
	def updateWeight(reaction_factor:Double){
		probaWeight = probaWeight * (1-reaction_factor) + reaction_factor * contributedError/count.max(1)// use max to avoid division by 0
		reset()
	}


	override def toString = move.toString
}
