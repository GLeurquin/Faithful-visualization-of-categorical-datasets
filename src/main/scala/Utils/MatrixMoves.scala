package thesis.utils

import thesis.orderings._
import thesis.rectangles._
import thesis.matrixTypes._

import scala.collection.mutable.ArrayBuffer

abstract class ReverseMove(){
	def reverseTheMove:Unit
	def moveName: String
}

/**
*	This trait allows to perform moves on the matrix,
*	and to reverse them efficiently
*/
trait MatrixMoves extends MyMatrixLike with ConvolutionUpdates{
	type Self <: MatrixMoves
	override def copy:Self // Abstract

	/**	For all columns in the given range, the values are flipped (1-->0 and 0-->1).
	*	The error is updated accordingly
	*
	*	@param rowRange: the range of row indices for which the bits should be flipped
	*	@return a ReverseMove object to reverse the flips and restore the error
	*/
	def flipBitsBetween(rowRange:Range):ReverseMove // Abstract
	
	/** Reverses all bits on the give row
	*	
	*	@param row: the range of row indices for which the bits should be flipped
	*	@return a ReverseMove object to reverse the flips and restore the error
	*/
	def flipBit(row: Int):ReverseMove = {
		flipBitsBetween(row to row)
	}

	/**
	*	@note post: swaps row a with row b, the error has been updated
	*	@return a function that reverses the move,
	*	assuming the state of the matrix when unapplying the move is the one that we obtain after applying this move
	*/
	def swap(a:Int, b:Int):ReverseMove = swap((a to a), (b to b))

	/**
	*	@note post: swaps row a with row b, the error has not been updated
	*	@return a function that reverses the move,
	*	assuming the state of the matrix when unapplying the move is the one that we obtain after applying this move
	*/
	def swapWithoutUpdate(a:Int, b:Int):ReverseMove = swap((a to a), (b to b), isReverse = true)

	/**
	*	@note pre: a.size == b.size || a.max+1 == b.min
	*		if they are of different sizes, a and b MUST be consecutive
	*	@note post: swaps block a with block b
	*		.. | . . . A . . . | . . B . . | ..  ----> .. | . . B . . | . . . A . . . | ..
	*	  	or
	*		.. | . . A . . | . . . B . . . | ..  ----> .. | . . . B . . . | . . A . . | ..
	*		or
	*		.. | . . A . .| . . . | . . B . . | .. ----> .. | . . B . .| . . . | . . A . . | ..
	*		or
	*		.. | . . B . .| . . . | . . A . . | .. ----> .. | . . A . .| . . . | . . B . . | ..
	*	@note post: the convolution has been incrementally updated (if it existed before, otherwise it is fully computed)
	*	@return a function that reverses the move,
	*	assuming the state of the matrix when unapplying the move is the one that we obtain after applying this move
	*/
	def swap(a:Range, b:Range, isReverse:Boolean = false):ReverseMove = {
		assert(!a.isEmpty && !b.isEmpty, "The ranges can't be empty !")
		if(a.max+1 != b.min) require(a.size == b.size, "The sizes of the intervals must be equal if they are not consecutive")
		assert(a.min >= 0 && a.max < rows, s"The row index a($a) must be within the matrix row bounds")
		assert(b.min >= 0 && b.max < rows, s"The row index b($b) must be within the matrix row bounds")

		if(a == b) return identityMove

		assert(a.intersect(b).isEmpty, "The ranges can't overlap !")

		val backup_error = theError

		// Update the convolutedMatrix and the error
		if(!theError.isEmpty && !isReverse) {
			val delta = updateConvolution(a, b)
			theError = Some(getError + delta)
		}

    	val newBlockA = (b.max-a.size+1 to b.max)
		val newBlockB = (a.min until a.min + b.size)

		// Update the row positions
		val tempV = getRows.slice(a.min, a.max+1).clone

		var i = b.min
		while(i <= b.max){
			val newb = newBlockB.min + (i - b.min)
			getRows(newb) = getRows(i)
			i += 1
		}

		var j = a.min
		while(j <= a.max){
			val newa = newBlockA.min + (j - a.min)
			getRows(newa) = tempV(j - a.min)
			j += 1
		}

	//	assume(getRows.deep==getRows.distinct.deep,s"row positions should be all different ${getRows.mkString("[",",","]")}")

		new ReverseMove(){
			def reverseTheMove = {
				theError = backup_error
				swap(newBlockB,newBlockA, isReverse=true)
			}
			def moveName = "Swap"
		}
	}

	/** Reverse move that does nothing
	*/
	val identityMove = new ReverseMove(){
		def reverseTheMove = Unit
		def moveName = "identity"
	}

	/**
	* 	@param k Number of rows which maximum error that must be found
	*/
	def apply_kmax(k: Int) = {
		val backupRows = getRows.clone
		val backupError = theError

		val kMax = new KMaxOrdering(k)
		kMax.changeOrdering(this)

		new ReverseMove(){
			def reverseTheMove = {
				permute(backupRows, false)
				theError = backupError // Must restore the error AFTER setting getRows !
			}
			def moveName = "KMax"
		}
	}

	/** Permutes the rows according to the ordering, and updates the rowPosition
	*
	*	@param newOrdering The new ordering of the rows
	*	@param mapping Whether the mapping should be applied to the newOrdering or not
	* 	@example
	* 	matrix m: 											0 1 2 3 4 (does not change)
	*	orderedMatrix before (rows): (rowPosition) 			3 4 2 1 0
	*	newOrdering = 										0 2 3 1 4
	*	orderedMatrix after (rows): (newrowPosition)		3 2 1 4 0
	*
	*	NEWorderedMatrix(0) = OLDorderedMatrix(0) = OLDorderedMatrix(newOrdering(0))
	*	NEWorderedMatrix(1) = OLDorderedMatrix(2) = OLDorderedMatrix(newOrdering(1))
	*	NEWorderedMatrix(2) = OLDorderedMatrix(3) = OLDorderedMatrix(newOrdering(2))
	*	NEWorderedMatrix(3) = OLDorderedMatrix(1) = OLDorderedMatrix(newOrdering(3))
	*	NEWorderedMatrix(4) = OLDorderedMatrix(4) = OLDorderedMatrix(newOrdering(4))
	*
	*	@note post: the error can't be computed incrementally, and is thus invalidated
	*/
	def permute(newOrdering:Array[Int], mapping: Boolean = true):ReverseMove = {
		require(newOrdering.length == rows, "the ordering must be of the same length as the number of rows")
		val rowsBackup = getRows
		val backup_error = theError // Must be before getRows=... otherwise will be set to None

		if(mapping) getRows = newOrdering.map{y => getRows(y)}
		else getRows = newOrdering

		theError = None

		new ReverseMove(){
			def reverseTheMove = {
				getRows = rowsBackup
				theError = backup_error // Must restore the error AFTER setting getRows !
			}
			def moveName = "newOrdering"
		}
	}

	/** Reverses the order of the rows between 2 random row indices
	*	
	*	@param a The first row index
	*	@param b The second row index 
	*	@return a function that reverses the move,
	*	assuming the state of the matrix when unapplying the move is the one that we obtain after applying this move
	*/
	def reverse(a: Int, b: Int):ReverseMove = {
		var i = a
		var j = b

		if(i > j){
			val t = i
			i = j
			j = t
		}

		val errorBackup = theError

		// Update the convolutedMatrix and the error
		if(!theError.isEmpty) {
			val delta = updateErrorReverse((i to j))
			theError = Some(getError + delta)
		}

		val nswaps = (j+1 - i)/2
		val reverseSwaps = new Array[ReverseMove](nswaps)

		var idx = nswaps-1
		while(i < j){
			reverseSwaps(idx) = swapWithoutUpdate(i, j)
			i += 1
			j -= 1
			idx -= 1
		}

		new ReverseMove(){
			def reverseTheMove = {
				var i = 0
				while(i < reverseSwaps.size){
					reverseSwaps(i).reverseTheMove
					i += 1
				}
				theError = errorBackup
			}
			def moveName = "Reverse"
		}
	}

	/** Shifts the rows a to b (inclusive) forward(if a<b)/backwards(if a>b) by a k rows
	*	
	*	@param a The first row index
	*	@param b The second row index
	*	@param k The shift amount
	*	@return a function that reverses the move,
	*	assuming the state of the matrix when unapplying the move is the one that we obtain after applying this move
	*/
	def relocate(a:Int, b:Int, k:Int):ReverseMove = {
		require(isDefinedAtRow(a), s"The row index a must be within the matrix row bounds")
		require(isDefinedAtRow(b), s"The row index b must be within the matrix row bounds")


		if(a == b || k == 0) return identityMove

		val d = if (a < b) k
				else -k
		val s = if (d > 0) a
				else b
		val e = if (d > 0) b+1
				else a+1

		val se_block 		= (s until e)
		val se_movedBlock   = if (d > 0) (e until e+d)
							  else (s+d until s)

		require(!se_block.isEmpty, s"the end must be after the start! s:$s, e:$e, d:$d")
		require(!se_movedBlock.isEmpty, s"the end of the new block must be after the start! s:$s, e:$e, d:$d")
		require(se_movedBlock.max <= rows, s"can't have Cyclical s:$s, e:$e, d:$d")
		require(se_movedBlock.min >= 0, s"can't have Cyclical s:$s, e:$e, d:$d")

		val reverseSwap = if(d>0) swap(se_block,se_movedBlock) else swap(se_movedBlock,se_block)

		new ReverseMove(){
			def reverseTheMove = {
				reverseSwap.reverseTheMove
			}
			def moveName = "Relocate"
		} // Relocate in the opposite order
	}

	/**
	*	@return true if this matrix contains the rectangle r (if it is in the bounds)
	*/
	def containsRectangle(r:Rectangle):Boolean = {
		val rr = r.toRowRange
		val rc = r.toColRange
		isDefinedAtRow(rr.min) && isDefinedAtRow(rr.max) && isDefinedAtCol(rc.min) && isDefinedAtCol(rc.max)
	}

	/**	Moves rectangle rec1 close to rectangle rec2 (rec2 does not move)
	*	@param rec1 the first rectangle
	*	@param rec2 the second rectangle
	*	@return a ReverseMove object to reverse this move and its associated error
	*/
	def swapRectangles(rec1:Rectangle, rec2:Rectangle):ReverseMove = {

		var r1 = rec1
		var r2 = rec2

		if(rec2.isBelow(rec1)){
			r2 = rec1
			r1 = rec2
		}

		if(r1.overlapsRows(r2) || r1.touchRows(r2)) return identityMove

		val x = r1.toRowRange

		val rev =  relocate(x.max,x.min, r1.topLeft.row-1 - r2.bottomRight.row)

		new ReverseMove(){
			def reverseTheMove = {
				rev.reverseTheMove
			}
			def moveName = "SwapRectangles"
		}
	}

	/** Shuffles the rows of this matrix
	*	
	*	@return this matrix
	*/
	def shuffle:this.type = {
		val order = Utils.rand.shuffle((0 to rows-1)).toArray
		permute(order)
		this
	}

	/** Shuffles the rows and columns of this matrix
	*
	*	@return this matrix
	*/
	def shuffleAll:this.type = {
		shuffle.t.shuffle.t
	}

	/** Shuffles percRows*rows rows
	*	
	*	@param percRows The percentage of rows to shuffle
	*	@return this matrix
	*/
	def shuffle(percRows: Double): this.type = {
		val num = Math.floor(percRows*rows).toInt // number of adjacent rows to shuffle
		if(num == 0) return this

		val a = randomRow
		val b = Math.min(randomRow+num,rows-1)

		shuffle(Utils.rand.shuffle((a to b).toList).toArray)
	}

	/** Shuffle percRows*rows rows and percCols*Cols cols
	*	
	*	@param percRows The percentage of rows to shuffle
	*	@param percCols The percentage of columns to shuffle
	*	@return this matrix
	*/
	def shuffle(percRows: Double, percCols: Double): this.type = {
		shuffle(percRows).t.shuffle(percCols).t
	}

	/** Shuffles the rows in array
	*	
	*	@param array The row indices
	*	@return this matrix
	*/
	def shuffle(array: Array[Int]): this.type = {
		var count = -1
		val newOrder = Array.tabulate(rows){ case i =>
			if(array.contains(i)){
				count += 1
				 getRows(array(count))
			}
			else getRows(i)
		}
		getRows = newOrder
		theError = None
		this
	}

	/** Mirrors this matrix on its horizontal axis (upside down)
	*/
	def upsideDown:this.type = {
		val order = (0 until rows).reverse.toArray
		permute(order)
		this
	}

	/**
	* 	@param a is a mapping of pairs (color1, color2) -> count
	*			where count is the number of times that the pair (color1, color2) appears
	*	@param forbiddenColors The colors that can't be selected as one that appears often
	* 	@return the pair of colors that appears the most often
	*/
	def getColors(a: Array[Array[Double]], forbiddenColors: ArrayBuffer[Double]) = {
		var max = 0.0
		var r = (-1,-1)

		for(i <- 0 until nBinaryMatrices; j <- 0 until nBinaryMatrices){
			if(a(i)(j) > max && !forbiddenColors.contains(i) && !forbiddenColors.contains(j)){
				max = a(i)(j)
				r = (i,j)
			}
		}
		r
	}

	/** Changes the color of rectangle rec from colorBefore to colorAfter
	*	
	*	@param rec The rectangle whose colors must be changed
	*	@param colorBefore The old color
	*	@param colorAfter The new color
	*/
	def changeColorRectangle(rec: Rectangle, colorBefore: Int, colorAfter: Int) = {
		select(colorBefore)
		updateRectangleError(rec,add=false)
		select(colorAfter)
		updateRectangleError(rec,add=false)

		var r = rec.topLeft.row
		while(r <= rec.bottomRight.row){
			var c = 0
			while(c < cols){
				select(colorBefore)
				if(this(r,c) != 0.0){
					val temp = this(r,c)
					this(r,c) = 0.0
					select(colorAfter)
					this(r,c) = temp
				}
				else if({
					select(colorAfter)
					this(r,c) != 0.0
				}){
					val temp = this(r,c)
					this(r,c) = 0.0
					select(colorBefore)
					this(r,c) = temp
				}
				c += 1
			}

			r += 1
		}

		select(colorBefore)
		updateRectangleError(rec, add=true)
		select(colorAfter)
		updateRectangleError(rec, add=true)
	}

	/**	If r1.width > r2.width changes the color of rectangle R2 so that it has the same color as R1
	* 	If r2.width > r1.width changes the color of rectangle R1 so that it has the same color of R2
	*	
	*	@param r1 The first rectangle
	*	@param r2 The second rectangle
	*	@return a ReverseMove to be able to reverse this change
	*/
	def changeColorRectangles(r1: Rectangle, r2: Rectangle):ReverseMove = {

		if(this.dontChangeColors) return identityMove

		var colorR1 = r1.getColor
		var colorR2 = r2.getColor

		val backup_error = this.getError

		assert(colorR1 != -1 && colorR2 != -1, "Error while getting the colors of the rectangles")

		val theR = 	if(r1.width > r2.width) r2
					else r1

		if(colorR1 != colorR2){
			changeColorRectangle(theR, colorR1, colorR2)
		}

		new ReverseMove(){
			def reverseTheMove = {
				changeColorRectangle(theR, colorR2, colorR1)
				setError(backup_error)
			}
			def moveName = "changeColorRectangle"
		}
	}

	/** For each pair of consecutive columns (c1, c2) find the most frequent pair of
	* 	colors (color1, color2) and changes color2 to color1 in column c2
	*/
	def changeColors = {
		var colors = getMatrix
		for(j <- 1 until cols){
			val a = Array.fill(nBinaryMatrices,nBinaryMatrices){0.0}
			var i = 0
			while(i < rows){
				val idx1 = colors(i,j).toInt
				val idx2 = colors(i,j-1).toInt
				a(idx2)(idx1) += 1.0
				i += 1
			}

			/*
				x = color of the subcolumn to the left,
				y = color of the current subcolumn
				should change y in x and x in y
			*/
			val forbiddenColors = ArrayBuffer[Double]()
			var pair = getColors(a, forbiddenColors)
			var x = pair._1
			var y = pair._2
			forbiddenColors.append(x)

			while(x != -1 && y != -1){
				(0 until rows).foreach{ i =>
					if(colors(i,j) == x){
						select(x)
						update(i,j,0)
						select(y)
						update(i,j,1)
						colors(i,j) = y
					}
					else if(colors(i,j) == y){
						select(y)
						update(i,j,0)
						select(x)
						update(i,j,1)
						colors(i,j) = x
					}
				}

				pair = getColors(a, forbiddenColors)
				x = pair._1
				y = pair._2
				forbiddenColors.append(x)
			}
		}

		theError = None
	}


}
