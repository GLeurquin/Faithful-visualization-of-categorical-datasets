package thesis.matrixTypes

import thesis.utils._
import breeze.linalg._

import scala.collection.mutable.BitSet

object BinarySparseMatrix extends MatrixHelpers{

	/** Creates a binary sparse matrix from a densematrix
	*	Will convert the dense matrix in either a binary (if it contains only ones & zeros) or a categorical dataset
	*
	*	@param m The matrix to convert
	*	@param convolutionSettings The convolution settings to be used
	*	@param labelX The labels to use for the columns
	*	@param labelY The labels to use for the rows
	*	@param desc The description of the dataset
	*/
	def apply(m:DenseMatrix[Double],
		convolutionSettings:ConvolutionSettings = ConvolutionSettings(Kernels.gaussianBlurSmooth(3,3), ErrorType.Abs, Padding.ZerosAndOnes),
		labelX:Array[String] = null,
		labelY:Array[String] = null,
		desc: String = "Default description"
	):BinarySparseMatrix = {
		new BinarySparseMatrix(m.rows, m.cols, m.toOneHotEncoding, convolutionSettings, labelX, labelY, desc)
	}
}

/**	Class representing a binary or a categorical dataset
*	
*	@constructor Creates a binary or categorical dataset from an array of bitsets
*	@param initRows The number of rows
*	@param initCols The number of columns
*	@param originalSparseMatrix An array of bitset, one for each category, representing the dataset in a flattened way
*	@param convolutionSettings The convolution settings to be used
*	@param labelX The labels to use for the columns
*	@param labelY The labels to use for the rows
*	@param desc The description of the dataset
*/
class BinarySparseMatrix(
		val initRows:Int,
		val initCols:Int,
		var originalSparseMatrix:Array[BitSet],
		var convolutionSettings:ConvolutionSettings = ConvolutionSettings(Kernels.gaussianBlurSmooth(3,3), ErrorType.Abs, Padding.ZerosAndOnes),
		var labelX:Array[String] = null,
		var labelY:Array[String] = null,
		val desc: String = "Default description"
	) extends MyMatrixLike with ConvolutionUpdates with MatrixMoves{


	type Self = BinarySparseMatrix

	val nBinaryMatrices = originalSparseMatrix.length // Number of binary matrices. Corresponds to the max number of unique attributes for a category

	/**
	*	@param hash: an index in the flattened matrix, equal to rows * cols + initCols
	*	@return the corresponding row and column indices
	*/
	def inverseHash(hash:Int):Array[Int] = {
		val r = hash / initCols;
		val c = hash - (r * initRows);
		Array(r,c)
	}

	init() // Init MyMatrixLike

	/**
	*	@inheritdoc
	*/
	def copy = copy(originalSparseMatrix)

	/**
	*	@param newSet the data to use in the copy
	*	@return an independent copy of this matrix using the data provided in newSet
	*/
	def copy(newSet:Array[BitSet]):BinarySparseMatrix = {
		val newSparse = new BinarySparseMatrix(initRows, initCols, newSet.map(_.clone), convolutionSettings.copy, labelX, labelY, desc)

		// Stack deep copy
		newSparse.realMappedRows = realMappedRows.map(_.copy)
		// Stack deep copy
		newSparse.realMappedCols = realMappedCols.map(_.copy)

		newSparse.theError = theError
		newSparse.select(selected)
		newSparse.dontChangeColors = dontChangeColors

		if(isTranspose) newSparse.t else newSparse
	}

	/**
	*	@inheritdoc
	*/
	def getRawValue(r:Int, c:Int):Double = {
		val hash = Utils.getHash(r,c, initCols)
		if(originalSparseMatrix(selected).contains(hash)) 1.0 else 0.0
	}

	/**
	*	@inheritdoc
	*/
	def update(r:Int, c:Int, v: Double) = {
		assert(isDefinedAtRow(r) && isDefinedAtCol(c), s"($r, $c) is out of bounds")

		val hash = 	if(isTranspose) Utils.getHash(getCols(c), getRows(r), initCols)
					else Utils.getHash(getRows(r), getCols(c), initCols)
		if(v == 1.0) originalSparseMatrix(selected) += hash
		else if(v == 0.0) originalSparseMatrix(selected) -= hash
		else assert(false, "v should be binary")
	}

	/**
	*	@inheritdoc
	*/
	def flipBitsBetween(rowRange:Range):ReverseMove = {
		require(isDefinedAtRow(rowRange.min) && isDefinedAtRow(rowRange.max))

		var getCoord = if(isTranspose) (coord:Array[Int]) => coord.tail
					   else (coord:Array[Int]) => coord.head

		val toRemove = 	originalSparseMatrix(selected).filter(x=>rowRange.contains(getCoord(inverseHash(x))))

		val toAdd:Set[Int] = rowRange.flatMap{r =>
			(0 to cols).flatMap{c =>
				val (x,y) = if(isTranspose) (c,r) else (r,c)
				val hash = Utils.getHash(x,y, initCols)
				if(toRemove.contains(hash)) None
				else Some(hash)
			}
		}.toSet

		def getAccumulatedError:Double = {
			rowRange.foldLeft(0.0){ case (acc, r) =>
				acc + (0 until cols).foldLeft(0.0){case (accCol, c) =>
					accCol + convoluteSinglePoint(r,c)
				}
			}
		}

		def updateError(f: => Unit):(Double, Double) = {
			val errorToRemove = getAccumulatedError
			f
			val errorToAdd = getAccumulatedError

			theError = Some(getError + errorToAdd - errorToRemove)
			(errorToRemove, errorToAdd)
		}

		if(!theError.isEmpty) {
			val (errorToRemove, errorToAdd) = updateError(() => originalSparseMatrix(selected) ++= toAdd --= toRemove)

			new ReverseMove(){
				def reverseTheMove = {
					originalSparseMatrix(selected) --= toAdd ++= toRemove
					theError = Some(getError - errorToAdd + errorToRemove)
				}

				def moveName = "update error"
			}
		}
		else new ReverseMove(){
			def reverseTheMove = {
				def f = originalSparseMatrix(selected) --= toAdd ++= toRemove
				if(!theError.isEmpty) updateError(f)
				else f
			}

			def moveName = "update error"
		}
	}
}
