package thesis.orderings
import thesis.matrixTypes._
import thesis.utils._
import thesis.UI._

import breeze.plot.Figure

case class GraphConfig(proba:Boolean, improvement:Boolean, error:Boolean, showProgress:Boolean)

/**	Performs a local search using the provided moves. Adapts the weights of these moves dynamically
*	
*	@param MAXIT The maximum number of iterations
*	@param computeEnergy The function to minimize
*	@param moves A list of moves with their weighted probability. The weights need not sum up to 1, as they will be normalized
*	@param segment_length Length of a segment, after which the weights of the moves will be updated
*	@param reaction_factor If equal to 1, will ignore weights of previous segment when updating for the next segment. If set to 0, the weights will not be adjusted.
*	@param autostopGradient If positive, disables automatic stop. If negative, LS will stop when the error gradient of the last segment(which is negative) is greater than this value
*	@param onlyRows Only reorder the rows ?
*	@param moveUpdate Should the weights of the moves be updated dynamically ?
*/
class LocalSearch(	var MAXIT:Int,
					moves:Array[MoveProba],
					progressUpdate:(Double, Double)=>Unit=(d:Double,e:Double) => Unit,
					isCanceled: ()=>Boolean = ()=>false,
					graphConfig:GraphConfig=GraphConfig(false, false, false, false),
					segment_length:Int=100,
					reaction_factor:Double=0.6,
					autostopGradient:Double = -0.001,
					onlyRows: Boolean = false,
					moveUpdate:Boolean = true
				)
extends MatrixOrdering with CanSetIterations with UsesKernelOptimization with MatrixHelpers{

	require(moves.length > 0, "There must be at least one move")
	require(reaction_factor <= 1 && reaction_factor >= 0, "Reaction factor must be between 0 and 1")
	var it = 0

	override def toString:String = {
		val moveDescription = s"${moves.map(_.move).mkString("-")}"
		s"LS-$moveDescription-${it}it-${if(moveUpdate) "" else "NoUpdate"}"
	}

	/**
	*	@inheritdoc
	*/
	override def setIterations(it:Int):Unit = {MAXIT = it}

	/**
	*	Sets the weights of the moves such that their sum equals 1.0
	*/
	private def normalizeMoves(moveList:Array[MoveProba]) {
		val TOTAL_WEIGHT = moveList.foldLeft(0.0){_ + _.probaWeight} // Compute the total weights
		moveList.foreach{_ *= 1/TOTAL_WEIGHT} // Normalize the weights so they are all between 0 and 1, and their sum equals 1
	}

	/**	Computes accumulative probabilites from the weights of the moves
	*/
	private def getAccumulativeMoves = {
		normalizeMoves(moves)
		moves.foreach{_ += 0.15} // Add something to prevent moves from having 0 weight, and disappearing from the search forever
		normalizeMoves(moves)

		moves.scanLeft(MoveProba(Move.Swap, 0.0)){ case (a,b) => // The first move in the initialisation does not matter
			MoveProba(b.move, a.probaWeight + b.probaWeight) // Sum up the probas gradually, so we can later make a probabilistic choice
		}.drop(1) // Drop the first one, because it's the initialisation of scanLeft, and we don't need it. Anyways, 0.0 will never be greater than any proba
	}

	private var accumulativeMoves = getAccumulativeMoves

	/**	Selects a move according to their respective weights
	*
	*	@return the chosen move
	*/
	private def selectMove = {
		val p = Utils.rand.nextDouble
		val v = accumulativeMoves.indexWhere{case MoveProba(m, w) => w >= p}
		if(v < 0){
			val newV = accumulativeMoves.size - 1
			println(s"WARNING: the index in selectMove was equal to -1, changing it to $newV")
			newV
		}
		else v
	}

	private case class NeighborResult(reverseMove:ReverseMove, chosenMoveIdx:Int)

	/** Selects a random move according to the given weight distribution in moves
	*
	*	@note post: m is changed
	*	@param m The matrix on which to apply a move
	*	@return The list of functions to call if you want to reverse the effects of the applied moves
	*/
	private def nextNeighbor(m:MatrixMoves): NeighborResult = {
		/*
			Select a random move, with weighted probability
		 	It's an option, but it will never be None, since the last weight is always equal to 1.0, and thus w >= [0,1] will always be true, so we can call "get" on it directly
		*/
		val chosenMoveIdx = selectMove
		val chosenMove:Move = moves(chosenMoveIdx).move

		val willTranspose = if(onlyRows) false else Utils.rand.nextFloat <= (m.cols.toDouble/(m.rows+m.cols))

		if(willTranspose) m.t
		val reverseChosenMove = chosenMove(m) // Apply the move
		if(willTranspose) m.t

		val reverseMove = new ReverseMove(){
			def reverseTheMove = {
				if(willTranspose) m.t // Transpose the matrix
				reverseChosenMove.reverseTheMove // Unapply the move
				if(willTranspose) m.t // Transpose the matrix again
			}

			def moveName = reverseChosenMove.moveName
		}

		NeighborResult(reverseMove, chosenMoveIdx)
	}

	/**
	*	@inheritdoc
	*/
	override def changeOrdering(start: MatrixMoves) = {

		var m = start
		it = 0
		var segmentCounter = 0

		moves.foreach{_.reset()} // reset score counters

		var best = m.copy
		var bestEnergy = m.getError
		var currentEnergy = m.getError

		val figure = if(graphConfig.showProgress) Figure("Progress") else null

		/**
		*	Graphical updates
		*/
		val probaGraph = if(graphConfig.proba && moves.length>1) new BarGraph("Proba/Move", "Proportion", moves.map(_.toString)) else null
		val improvementGraph = if(graphConfig.improvement && moves.length>1) new BarGraph("Improvement/Move", "Cumulative Improvement", moves.map(_.toString)) else null

		def updateProbaGraph() {
			if(graphConfig.proba && moves.length>1) moves.zipWithIndex.foreach{case(m, idx) =>
				probaGraph(m.toString) = m.probaWeight
			}
		}

		updateProbaGraph // Call it once at the beginning

		val cyclicArray = new CyclicArray(segment_length) // Contains the errors within some history, to automatically stop when the gradient is too low

		var stop = false
		while(it < MAXIT && !isCanceled() && !stop){
			progressUpdate(it.toDouble/MAXIT, bestEnergy)
			cyclicArray += bestEnergy

			/**
			* 	Choose and perform the move
			*/

			val res = Utils.time{nextNeighbor(m)}
			val neighborResult = res.result
			val chosenMove = moves(neighborResult.chosenMoveIdx)
			chosenMove.count += 1
			chosenMove.time += res.time


			/**
			*	Evaluate if it was a good choice or not
			*/
			currentEnergy = m.getError

			val delta = bestEnergy - currentEnergy // We want this to be positive, to minimize the energy

			// Minimize the energy
			if(delta >= 0){
				bestEnergy = currentEnergy // Update the best energy
				chosenMove.contributedError += delta // Add the contribution of the error to this move
				if(graphConfig.improvement && moves.length>1) improvementGraph += (chosenMove.toString, delta)
			}
			else{
				neighborResult.reverseMove.reverseTheMove // reverse the move
			}

			/**
			*	Move weights update
			*/
			if(segmentCounter >= segment_length){
				segmentCounter = 0 // Reset the counter

				if(moveUpdate){
					moves.foreach{_.updateWeight(reaction_factor)} // learn new probabilities
					accumulativeMoves = getAccumulativeMoves // Update the accumulative moves
					updateProbaGraph()
				}

				if(graphConfig.showProgress) visualizeRescaled(m, "Progress", oldFig=Some(figure))

				val gradient = cyclicArray.gradient
				if(gradient <= 0 && gradient > autostopGradient) stop = true // Automatic stop
			}

			segmentCounter += 1
			it += 1
		}

		progressUpdate(1.0, bestEnergy)
		m
	}

}
