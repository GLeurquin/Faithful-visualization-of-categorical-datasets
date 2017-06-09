package thesis.orderings
import thesis.matrixTypes._
import thesis.utils._
import thesis.rectangles._

import breeze.linalg._

/** Cuts the rows of the matrix into nBlocks and then aggregate them to get a
* 	matrix of nBlocks rows. Use ordering to rearrange those rows and finally
* 	apply this ordering to the original matrix
*	@param nBlocks the number of blocks in which to cut the matrix
*	@param squash a function that can squash a column into a single value
*	@param ordering The ordering to use to rearrange the squashed rows
*/
class BlockOrdering(nBlocks:Int = 16, squash:(Array[Double])=>Double, ordering:MatrixOrdering) extends MatrixOrdering {
	require(nBlocks >= 1, "The number of blocks must be greater than 1")

	override def toString = s"BlockOrdering($nBlocks blocks)"

	private val DEBUG = false

	private var row_ranges:Array[Rectangle] = null
	private var blockSize = 0

	/**	Creates a new matrix from m, with nBlocks (or nBlocks+1) rows
	*	Where each row is the squashed result of m.rows/nBlocks (except for the last block) rows of the matrix m
	*	
	*	@param m the matrix to squash
	*/
	private def getSquashedMatrix(m:MatrixMoves):MatrixMoves = {
		blockSize = m.rows/nBlocks.max(1) // Number of rows per rectangle
		row_ranges = Utils.makeBatches((0 to m.rows), blockSize).sliding(2).map{case x =>
			Rectangle(Coord(x(0), 0), Coord(x(1)-1, m.cols-1))
		}.toArray

		val blockMatrix = DenseMatrix.tabulate[Double](row_ranges.length, m.cols){case(i, j) =>
			val rectangle = row_ranges(i) // [rmin, rmax]
			val colToSquash = Array.tabulate[Double](rectangle.height){r =>
				val rowIdx = rectangle.topLeft.row + r
				m(rowIdx, j)
			}
			squash(colToSquash)
		}

		BinarySparseMatrix(blockMatrix, m.getConvolutionSettings)
	}

	/**	
	*	@param orderedBlockMatrix The ordered matrix of squashed blocks
	*	@param m The matrix m on which to apply the ordering
	*	@return The ordering of the rows of m according to the ordering of the orderedBlockMatrix
	*/
	private def reinsertBlocks(orderedBlockMatrix:MatrixMoves, m:MatrixMoves):Array[Int] = {
		val blockOrdering = orderedBlockMatrix.getRows
		assert(orderedBlockMatrix.rows == row_ranges.length, "The number of the lines in the block matrix is not equal to the number of blocks")

		val newOrdering = blockOrdering.flatMap{newBlockNumber =>
			// Assumes that the original ordering of orderedBlockMatrix was 0,1,2,3,4,...

			val startIdx = newBlockNumber * blockSize
			val r = row_ranges(newBlockNumber)
			val endIdx = startIdx + r.height
			(startIdx until endIdx)
		}

		if(DEBUG){
			if(!blockOrdering.equals(Array.tabulate[Int](orderedBlockMatrix.rows){i=>i})){
				assert(!newOrdering.equals(m.getRows), "The block ordering changed, but not the matrix ordering!")
			}
			else{
				assert(newOrdering.equals(m.getRows), "The block ordering did not change, but the matrix ordering did!")
			}
		}

		assert(newOrdering.length == m.rows, "The ordering must be of length equal to the number of rows")

		newOrdering
	}

	/**
	*	@inheritdoc
	*/
	override def changeOrdering(m:MatrixMoves) = {
		if(m.isCat) throw new IllegalArgumentException("Block ordering only works for binary data")
		val squashedMatrix = getSquashedMatrix(m)
		val orderedBlockMatrix = ordering.changeOrdering(squashedMatrix)
		val newOrdering = reinsertBlocks(orderedBlockMatrix, m)
		if(DEBUG) assert(newOrdering.deep==newOrdering.distinct.deep)
		m.permute(newOrdering) // Discard result
		m
	}
}
