package thesis.utils

import thesis.matrixTypes._
import thesis.rectangles._

/** Enables convolution operations on matrices
*	Allows to incrementally compute the error when the order of the rows changes.
*/
trait ConvolutionUpdates extends MyMatrixLike with MatrixHelpers{
	type Self <: ConvolutionUpdates
	override def copy:Self // Abstract

	private val CONVO_DEBUG = false

	protected var convolutionSettings:ConvolutionSettings
	private def theKernel:Kernel =  convolutionSettings.kernel

	// padding function
	private var getPadding:(Int,Int, (Int)=>Int)=>Double = getPaddingFunction(convolutionSettings.padding)

	/** Changes the convolution settings for this matrix and updates the error accordingly
	*	@param newConvolutionSettings The new convolution settings
	*/
	def setConvolutionSettings(newConvolutionSettings:ConvolutionSettings){
		if(!convolutionSettings.equals(newConvolutionSettings)){
			convolutionSettings = newConvolutionSettings.copy // Make a copy here, just to make sure we have a unique reference to the kernel
			if(this.isTranspose != convolutionSettings.kernel.isTranspose) convolutionSettings.kernel.t // keep kernel and matrix synchronized on the transpose
			computeSingleError = Utils.getSingleErrorFunction(convolutionSettings.errorType)
			getPadding = getPaddingFunction(convolutionSettings.padding)
			theError = None
		}
	}

	def getConvolution = getMatrix.convolute(convolutionSettings)
	def getConvolutionSettings = convolutionSettings

	/**
	*	@inheritdoc
	*/
	override def t:this.type = {
		assert(this.isTranspose == theKernel.isTranspose, s"The kernel is ${if(theKernel.isTranspose) "" else "not"} transposed, but the matrix is ${if(this.isTranspose) "" else "not"} transposed!")
		theKernel.t
		super.t
	}

	/**
	*	@return the error corresponding to the difference (between the matrix and its convolution) given in argument
	*/
	var computeSingleError = Utils.getSingleErrorFunction(convolutionSettings.errorType)

	/** The padding must be the same function for the top and left side,
	*	and the same (possibly different from the first) function for the bottom and right side
	*	That way the padding will be coherent when transposing.
	*	
	*	@param padding The padding to use
	*	@return A function that can compute the padding for some given coordinates, using a convert function to get coordinates after the move
	*/
	private def getPaddingFunction(padding:Padding):(Int,Int, (Int)=>Int)=>Double = {
		padding match {
    		case Padding.Boundary => {
				//Values on the edges are copied as far as
				// necessary to provide values for the convolution
            	(r:Int,c:Int, convert:(Int)=>Int) => {
					val newR = {
						val a = r.min(rows-1).max(0)
					    convert(a)
					}
					val newC = c.min(cols-1).max(0)

					getForPadding(newR,newC, (_, _) => {assert(false); 0.0})
				}
            }
			case Padding.BoundaryAsymmetric => {
				// same as Boundary except that the padding if r < 0 or c > cols - 1 is 0
            	(r:Int,c:Int, convert:(Int)=>Int) => {
		            val newR = {
						val a = r.min(rows-1)
						convert(a)
					}
					val newC = c.max(0)
					if(newR < 0 || newC > cols - 1) 0.0
				    else getForPadding(newR,newC, (_, _) => {assert(false); 0.0})
			    }
            }
            case Padding.Zero => (r:Int,c:Int, convert:(Int)=>Int) => {0.0}
            case Padding.ValueOpt(v: Double) => (r:Int,c:Int, convert:(Int)=>Int) => {v}
			case Padding.ZerosAndOnes => (r:Int,c:Int, convert:(Int)=>Int) => {
				if(r < 0 || c < 0) 1.0 else 0.0
			}
            case op => require(false, "cannot handle Padding value " + op); (r:Int,c:Int, convert:(Int)=>Int) => {0.0}
        }
	}

	/**
	*	@inheritdoc
	*/
	override def init() = {
		super.init()
    	require( cols * theKernel.matrix.cols != 0, "data and kernel must have a non-empty number of columns DenseMatrix")
    	require( rows * theKernel.matrix.rows != 0, "data and kernel must have a non-empty number of rows DenseMatrix")
	}

	/**
	* 	@param r rectangle for which the error of the rows must be updated
	* 	@param add whether the error for this rectangle should be added or removed
	*  	If add is true, add the error for this rectangle to the current error
	*  	otherwise remove it
	*	@note Only updates for the error for the currently selected matrix
	*/
	def updateRectangleError(r: Rectangle, add:Boolean) = {
		var delta = 0.0

		var i = math.max(0,r.topLeft.row-theKernel.rows/2)

		while(i < math.min(r.bottomRight.row + theKernel.rows/2 + 1,rows)){
			var c = 0

			while(c < cols){
				if(apply(i,c) != 0.0){
					val convo = convoluteSinglePoint(i,c)
					 delta += computeSingleError(apply(i,c), convo)
				}
				c += 1
			}

			i += 1
		}

		if(add) theError = Some(getError + delta)
		else theError = Some(getError - delta)
	}

	/**	Sets the error for the current matrix
	*	
	*	@param d The new error
	*/
	def setError(d: Double) = theError = Some(d)
	
	/**
	*	@return The error for the current matrix
	*/
	def getError:Double = {
		val res = theError match {
			case Some(x) => x
			case _ => {
				theError = Some(getFullConvolutionError)
				theError.get
			}
		}

		math.floor(res * 100000) / 100000 // deal with rounding problem
	}

    private def kernel = theKernel.matrix
	private def di = kernel.rows / 2 // floor division rows
	private def dj = kernel.cols / 2 // floor division cols
	private def inBlock = (0 until di)
	private def outOfBlock = (1 to di)

	private def kernelColRange = (-dj to dj)

	/**
	*	@return the padding that corresponds to the point (r,c), AFTER the rows have been swapped.
	*	On the currently selected matrix
	*/
	def getWithPadding(r:Int, c:Int, convert: Int => Int = (x:Int) => x):Double = {
		if(isDefinedAt(r,c)) apply(convert(r), c)
        else getForPadding(r,c, getPadding(_,_,convert))
	}

	/**
	* 	@return the convolution of point at position (r,c)
	*/
	def convoluteSinglePoint(r:Int, c:Int):Double = {
		var sum = 0.0
		var kc = 0
		while(kc < kernel.cols){
			var kr = 0
			while(kr < kernel.rows){
				if(getWithPadding(r + kr - di, c + kc - dj)!=0) sum += kernel(kr, kc)
				kr += 1
			}
			kc += 1
		}
		sum
	}

	/** 
	*	@return The error for all the matrices
	*/
	def getFullConvolutionError = {
		selectAll{(x) => getFullConvolutionErrorHelper}.sum
	}

	/**
	*	@param r The row for which the convolution must be computed
	*	@return The error for all the matrices for a particular row
	*/
	def getFullConvolutionErrorSingleRow(r: Int) = selectAll{(x) => getConvolutionErrorSingleRow(r)}.sum

	/**
	*	@param kernel the kernel to use. Currenlty only uneven kernels supported.
	*	@param padding the policy to apply on the edges of the matrix. (Cyclical, Boundary, Zero or ValueOpt)
	*	@note post: m is not modified
	* 	@return the convolution of the matrix and the kernel
	*/
	private def getFullConvolutionErrorHelper: Double = {
        require( cols * kernel.cols != 0, "data and kernel must have a non-empty number of columns DenseMatrix")
        require( rows * kernel.rows != 0, "data and kernel must have a non-empty number of rows DenseMatrix")
        require( kernel.rows % 2 == 1, "the kernel must have an odd number of rows" )
        require( kernel.cols % 2 == 1, "the kernel must have an odd number of cols" )
		var sumTot = 0.0
		var r = 0
		while(r < rows){
			sumTot += getConvolutionErrorSingleRow(r)
			r += 1
		}

		sumTot
 	} // End convolute

	/**
	*	@param r The row for which the convolution must be computed
	*	@return The convolution error for a single row for the currently selected matrix
	*/
	private def getConvolutionErrorSingleRow(r: Int): Double = {
		var c = 0
		var sumTot = 0.0
		while(c < cols){
			if(apply(r,c) != 0.0){
				val sum = convoluteSinglePoint(r,c)
				sumTot += computeSingleError(apply(r,c), sum)
			}

			c += 1
		}

		sumTot
	}

	/** Helper to compute the error when two blocks of rows are put next to each other
	* 	Compute the errors for the rows between init and limit by using the function convert before calling getWithPadding
	*	
	*	@param init Row index
	*	@param limit Row index
	*	@param convert Function to get the row after the swap from the row before the swap
	*/
	def partialErrorBlocks(init: Int, limit: Int, convert: Int => Int): Double = {
		assert(isDefinedAtRow(init), s"init=$init is out of the bounds [0,$rows]")
		assert(isDefinedAtRow(limit), s"limit=$limit is out of the bounds [0,$rows]")
		val nK = theKernel.rows
		var cK = theKernel.cols
        var r = init
        var sum = 0.0
        while(r <= limit){
            var c = 0
            while(c < cols){
				if(apply(r,c) != 0.0){
					var conv = 0.0
					var i = -nK/2
					while(i <= nK/2){
						var j = -cK/2
						while(j <= cK/2){
							conv += getWithPadding(convert(r+i), c+j) * kernel(i+nK/2, j+cK/2)
							j += 1
						}
						i += 1
					}
	                sum += computeSingleError(apply(r,c), conv)
				}
                c += 1
            }
            r += 1
        }
        sum
    }

    /** Computes the error when two blocks of rows are put next to each other
    *	
	*	@param rangeA range of row indices for the first block
	*	@param rangeB range of row indices for the second block
    */
 	def updateConvolution(rangeA:Range, rangeB:Range):Double = {
 		selectAll{x => updateConvolutionHelper(rangeA, rangeB)}.sum
 	}

	/**
	*	Note to self: DenseMatrix are indexed from top left
	*	To be called BEFORE the rows of the ordered matrix are swapped
	*	@param rangeA the span of the first set of rows to be swapped
	*	@param rangeB the span of the second set of rows to be swapped
	*	@note post The convolution matrix and the error were updated to correspond to the swap
	*	@return a delta of the convolution
	*/
	private def updateConvolutionHelper(rangeA:Range, rangeB:Range):Double = {
		if(rangeA.max+1 != rangeB.min) require(rangeA.size == rangeB.size, "The sizes of the intervals must be equal if they are not consecutive")
		assert(!rangeA.isEmpty && !rangeB.isEmpty, "The ranges can't be empty !")
		assert(rangeA.intersect(rangeB).isEmpty, "The ranges can't overlap !")

        val newBlockA = (rangeB.max-rangeA.size+1 to rangeB.max)
		val newBlockB = (rangeA.min until rangeA.min + rangeB.size)
		
		/** Converts the row index a, corresponding to a row before the swap, into the corresponding row index after the swap
		*
		*	@param a the row index before the swap
		*	@param isBlock if the row a is part of a block or not
		*/
		def convert(a:Int, isBlock:Boolean) = {
			if(isBlock && newBlockB.contains(a)) convertToB(a) // The point belongs to block A, but that block will be swapped to the block B, so use the point of the block B instead
			else if(isBlock && newBlockA.contains(a)) convertToA(a) // The point belongs to block B, but that block will be swapped to the block A, so
			else a
		}

		/** Converts the row index a, that is part of block B, into the corresponding row index in block A after the swap
		*	
		*	@note pre: a is a row in block B
		*	@return the row index of a after the swap
		*/
		def convertToA(a:Int) = {
			val offset = (a-newBlockA.min)
			rangeA.min + offset
		}

		/** Converts the row index a, that is part of block A, into the corresponding row index in block B after the swap
		*	
		*	@note pre: a is a row in block A
		*	@return the row index of a after the swap
		*/
		def convertToB(a:Int) = {
			val offset = (a-newBlockB.min)
			rangeB.min + offset
		}

		/**
		*	@return the list of rows for which the error must be updated, and the direction in which to update it
		*/
		def getListOfPointsToUpdate(block:Range, newBlock:Range, otherBlock:Range):IndexedSeq[(Int, Int, Boolean, Boolean)] = { // (row, UP)
			val topPoints = {
				inBlock.map(k => (block.min + k, newBlock.min + k, true,false)).filter{case(oldRow, newRow, lookUp,isOutOfBlock) => block.contains(oldRow)} ++
				outOfBlock.map{k => val r = block.min - k; (r, r, false,true)}.filter{case(oldRow, newRow, lookUp,isOutOfBlock) => !otherBlock.contains(oldRow)} // oldRow = newRow because we're out of the block, this row does not move
			}

			val bottomPoints = {
				inBlock.map(k => (block.max - k, newBlock.max - k, false,false)).filter{case(oldRow, newRow, lookUp,isOutOfBlock) => block.contains(oldRow)} ++
				outOfBlock.map{k => val r = block.max + k; (r, r, true,true)}.filter{case(oldRow, newRow, lookUp,isOutOfBlock) => !otherBlock.contains(oldRow)} // oldRow = newRow because we're out of the block, this row does not move
			}

			topPoints ++ bottomPoints
		}

		val it = (getListOfPointsToUpdate(rangeA, newBlockA, rangeB) ++ getListOfPointsToUpdate(rangeB, newBlockB, rangeA)).filter{case(oldRow, newRow, lookUp,isOutOfBlock) => isDefinedAtRow(oldRow)}.toSet.toList // remove duplicates

		var newIt = it.groupBy{_._1}.values.toList
		if(CONVO_DEBUG) newIt.foreach{println}
		if(CONVO_DEBUG) println("--------------")

		var bigDelta = 0.0

		var c = 0
		while(c < cols){
			var i = 0
			while(i < newIt.size){
				val l = newIt(i)

				val r = l(0)._1 // r and newR are constant in l
				val newR = l(0)._2

				if(CONVO_DEBUG) println(l)
				var delta = 0.0

				if(apply(r,c) != 0.0){
					var j = 0
					while(j < l.size){
						val oldRow = l(j)._1
						val newRow = l(j)._2
						val lookUp = l(j)._3
						val isOutOfBlock = l(j)._4

						// Apply the kernel, but in reverse
						val kernelRowRange = if(lookUp) (-di to -1) // For each row of the kernel above the half
											 else (1 to di) // For each row of the kernel below the half

						var kc = kernelColRange.min
						while(kc <= kernelColRange.max){
							var kr = kernelRowRange.min
							while(kr <= kernelRowRange.max){
								val oldRowK = oldRow + kr
								val pointNotMoved = isOutOfBlock && !(rangeA.contains(oldRowK) || rangeB.contains(oldRowK)) // No need to update the error points that did not move
								if (!pointNotMoved){ // if the point moved
									val theValueToRemove:Double = if(getWithPadding(oldRowK, c + kc)!=0) -1 * kernel(kr+di, kc+dj) else 0.0

									if(CONVO_DEBUG) println(s"Removing the contribution of (${oldRow + kr}, ${c + kc}): $theValueToRemove")

									lazy val isBlock = rangeA.contains(oldRow) || rangeB.contains(oldRow)

									val theValueToAdd:Double = if(getWithPadding(newRow + kr, c + kc, convert(_,isOutOfBlock || isBlock))!=0) kernel(kr+di, kc+dj) else 0.0

									if(CONVO_DEBUG) println(s"Adding the contribution of (${newRow + kr}, ${c + kc}): $theValueToAdd")

									delta += theValueToRemove + theValueToAdd
								}
								kr += 1
							}
							kc += 1
						}
						j += 1
					}

					val previousConvo = convoluteSinglePoint(r,c)
					if(CONVO_DEBUG){
						val newR = l(0)._2
						println(s"previousConvo in ($r,$c) : $previousConvo")
						println(s"delta = $delta")
						println(s"newConvo      in ($r,$c)->($newR,$c) : ${previousConvo+delta}")
						println()
					}

					val errorToAdd = computeSingleError(apply(r,c), previousConvo+delta)
					val errorToRemove = computeSingleError(apply(r,c), previousConvo)

					bigDelta += errorToAdd - errorToRemove
				}

				i += 1
			} // end while newIt
			c += 1
		}
		bigDelta
    } // End convolute update

    /*********************************
	*	Updates for the reverse move
    *********************************/

    /** Updates the error for the the reverse move for all matrices
    *
    * 	@param a Rows that have been reversed
    */
    def updateErrorReverse(a: Range):Double = {
 		selectAll{x => updateErrorReverseHelper(a)}.sum
 	}

	/** Updates the error when we reverse the rows in a for the current matrix
	*
	* 	@param a Rows that have been reversed
	*/
	private def updateErrorReverseHelper(a: Range): Double = {

		/**
		* 	@param i row for which we are recomputing the convolution
		* 	@param k row of the kernel that we are considering
		* 	@return the row in the reversed matrix need to compute the kth
		* 			row of the convolution of a point in row i
		*/
		def convert(i: Int, k: Int): Int = {
			val x = i + k
			val newX = a.max + a.min - x
			if(convolutionSettings.padding == Padding.Boundary && !isDefinedAtRow(newX)) i
			else newX
		}

		/** Helper to update the error of a single point
		*	
		* 	@param i row for which we are updating the error
		* 	@param c column for which we are updating the error
		* 	@return the delta for the convolution of point (i,c) before and after the reverse
		*/
		def updatePoint(i: Int, c: Int): Double = {
			val kernelRowRange = (-di to di)

			var kr = kernelRowRange.min
			var krMax = kernelRowRange.max

			// If some rows are the same before and after the reverse we don't have to
			// recompute their contribution to the convolution
			if(i < a.min){
				kr = a.min - i
				krMax = Math.min(a.max - i, kernelRowRange.max)
			}
			else if(i > a.max){
				kr = Math.max(kernelRowRange.min, a.min - i)
				krMax = a.max - i
			}

			var delta = 0.0
			while(kr <= krMax){
				var kc = kernelColRange.min
				while(kc <= kernelColRange.max){
					if(!(a.contains(i+kr) && a.contains(i))){
							delta -= getWithPadding(i + kr, c + kc) * kernel(kr+di, kc+dj)
							delta += getWithPadding(convert(i, kr), c + kc) * kernel(kr+di, kc+dj)
					}
					kc += 1
				}
				kr += 1
			}
			delta
		}

		/** Helper to create while loops
		*	
		* 	@param init initial value of the loop variable
		* 	@param c column for which we are updating the error
		* 	@param cond function that takes the loop variable as input and returns true when should stop
		* 	@param inc increment or decrement for the loop variable
		* 	@return a pair (r, bigDelta) where r is the value of the loop variable after the loop has finished
		* 	and bigDelta is the delta for the error for this column
		*/
		def whileLoop(init: Int, c: Int, cond: Int => Boolean, inc: Int): (Int, Double) = {
			var bigDelta = 0.0
			var r = init
			while(cond(r)){
				if(apply(r,c) != 0.0){
					var delta = updatePoint(r, c) //get change of convolution for point (r,c)
					val previousConvo = convoluteSinglePoint(r,c) //convolution of the point before the move is applied
					val errorToAdd = computeSingleError(apply(r,c), previousConvo+delta)
					val errorToRemove = computeSingleError(apply(r,c), previousConvo)
					bigDelta += errorToAdd - errorToRemove
				}
				r += inc
			}
			(r, bigDelta)
		}

		var c = 0
		var bigDelta = 0.0
		// update the error for all the columns
		while(c < cols){
			// Update rows above rows in a
			bigDelta += whileLoop(Math.max(a.min - theKernel.rows/2,0), c, i => i < a.min, 1)._2
			// Update rows in a (upper rows)
			val (k, res) = whileLoop(a.min, c, k => k < a.min + theKernel.rows/2 && k <= a.max, 1)
			bigDelta += res
			// Update rows in a (lower rows)
			bigDelta += whileLoop(a.max, c, f => f > a.max - theKernel.rows/2 && f >= k, -1)._2
			// Update rows below rows in a
			bigDelta += whileLoop(Math.min(a.max + theKernel.rows/2, rows-1), c, j => j > a.max, -1)._2
			c += 1
		}
		bigDelta
	}
}
