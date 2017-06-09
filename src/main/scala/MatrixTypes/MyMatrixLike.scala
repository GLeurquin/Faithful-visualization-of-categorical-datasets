package thesis.matrixTypes

import thesis.utils._
import thesis.rectangles._


import breeze.linalg._
import scala.reflect.ClassTag
import scala.collection.mutable.ArrayBuffer

/**	Class that allows the creation of submatrices
*
*	@param range Represents the indices in the original matrix that are represented by the smaller array "array"
*	@param array Represents the order of the rows or columns of the matrix for this range of rows/columns
*	@param backupError The error of the matrix before the submatrix is made
*/
case class StackMapping(range:Range, array:Array[Int], backupError:Option[Double]){
	assert(range.length == array.length)
	def getArray = array
	def copy = StackMapping(range, array.clone(), backupError)
	override def toString = s"StackMapping($range)"
}

/**
*	Abstract trait
*	Allows a number of useful modifications on matrices
*	In particular:
*	Making submatrices
*	Create tips using labels
*	Reorder the rows and columns
*	Save the state to a .backup
*/ 
trait MyMatrixLike{
	type Self <: MyMatrixLike

	/**
	*	@return an independent copy of this matrix
	*/
	def copy:Self // Abstract

	val desc:String  // Abstract
	val nBinaryMatrices:Int // Abstract

	/**
	*	@return true if this matrix is categorical
	*/
	def isCat:Boolean = nBinaryMatrices > 1

	val initRows:Int // Abstract
	val initCols:Int // Abstract

	/*******************************************************
	*	Variables (need to copy them to get a new instance)
	*******************************************************/
	var labelX:Array[String] // Abstract
	var labelY:Array[String] // Abstract

	protected var theError:Option[Double] = None

	/**
	*	Setting this to true means that the attributes for the categories are common accross the whole matrix.
	*/
	var dontChangeColors = false // If set to true, the colors of this matrix won't be changed

	private var _selected = 0 // color that is selected 
	private var isTransposeVar = false
	def isTranspose = isTransposeVar

	// protected

	protected var realMappedRows = new ArrayBuffer[StackMapping]() // Always refers to original ROWS
	realMappedRows += StackMapping((0 until initRows), Array.tabulate(initRows)(x => x), theError)

	protected var realMappedCols = new ArrayBuffer[StackMapping]()// Always refers to original COLS
	realMappedCols += (StackMapping((0 until initCols), Array.tabulate(initCols)(x => x), theError))


	def selected = _selected
	
	/** Selects the binary matrix representing category i
	*	
	*	@param i The category to select
	*/
	def select(i:Int) {
		require(i < nBinaryMatrices && i >= 0)
		_selected = i
	}

	/**	Applies f to all binary matrices
	*
	*	@param f the function to apply
	*	@return an array with the results of f for each category
	*/
	def selectAll[X: ClassTag](f:(Int) => X):Array[X] = {
		val selectedBackup = selected
		val resultArray:Array[X] = Array.tabulate(nBinaryMatrices){ idx =>
			select(idx)
			f(idx)
		}
		select(selectedBackup)
		resultArray
	}

	/*******************************
		ONLY FOR DEBUG AND TESTING
	********************************/

	/**
	*	@return A representation of this matrix as a densematrix with values equal to the categories
	*/
	private def getMatrixHelper:DenseMatrix[Double] = {
		// println("SparseMatrix: Only use getConvolution for debug & testing")
		DenseMatrix.tabulate[Double](rows,cols){case(i,j) => apply(i, j)}
	}

	/**
	*	@param matrices a set of binary matrices. Each of them has one component of the binary vector representing a color
	*	@example if there are 3 matrices, then the color of point (0,0) will be the vector (v1,v2,v3) = (mat(0)(0,0),mat(1)(0,0),mat(2)(0,0))
	*	@return a matrix where the color is represented by a number (ex: v1^2 + v2^1 + v3^0)
	*/
	def getMatrix: DenseMatrix[Double] = {
		val m = new DenseMatrix[Double](rows, cols)

		if(!isCat) return getMatrixHelper

		val selectedBackup = selected

		for( i <- 0 until rows; j <- 0 until cols) {
			val idx = (0 until nBinaryMatrices).indexWhere(x => {
				select(x)
				apply(i,j) != 0.0
			})
			assert(idx >= 0)
			m(i,j) = idx
		}

		select(selectedBackup)

		m
	}

	/**
	*	@param r the row of the mapped matrix (which might be transposed)
	*	@param c the col of the mapped matrix (which might be transposed)
	*	@return a tuple where the first int is the row in the original matrix and the second int is the col in the original matrix
	*/
	protected def getTuple(r:Int, c:Int):Array[Int] = {
		if(isTransposeVar) Array(getCols(c), getRows(r))
		else  Array(getRows(r), getCols(c))
	}

	/**
	*	@param r the row of the mapped matrix (which might be transposed)
	*	@param c the col of the mapped matrix (which might be transposed)
	* 	@return the value of (row,col) in the current matrix (possibly transposed)
	*/
	def apply(r:Int, c:Int):Double = {
		assert(isDefinedAtRow(r) && isDefinedAtCol(c), s"($r, $c) is out of bounds")
		if(isTransposeVar) getRawValue(getCols(c), getRows(r))
		else getRawValue(getRows(r), getCols(c))
	}

	/**
	*	@param r The row in the original matrix
	*	@param c The column in the original matrix
	*	@return the value at position r,c in the original matrix, whithout going through the mapping
	*/
	def getRawValue(r:Int, c:Int):Double // abstract

	/** Updates the value at (r,c) to v
	*	
	*	@param r The row
	*	@param c The column
	*	@param v The value to set at (r,c)
	*/
	def update(row:Int, col:Int, value: Double) // Abstract

	/**
	*	@return a random row index
	*/
	def randomRow = Utils.rand.nextInt(rows)
	
	/**
	*	@return a random column index
	*/
	def randomCol = Utils.rand.nextInt(cols)

	/**	Same as realMappedRows, except that these represent the latest submatrix
	*	
	*	@return The mapping of the rows for the current matrix
	*/
	def mappedRows = realMappedRows.last.getArray
	
	/**	Same as realMappedCols, except that these represent the latest submatrix
	*	
	*	@return The mapping of the columns for the current matrix
	*/
	def mappedCols = realMappedCols.last.getArray

	def isFullMatrix = realMappedRows.length == 1 && realMappedCols.length == 1 // True if this matrix is not a submatrix

	/** If row or col are out of the current matrix, try to get the value from the previous submatrix
	*	If row and col are still out of bounds in the original matrix, uses the paddingFunc
	*
	*	@param row The row for which the padding may have to be fetched
	*	@param col The column for which the padding may have to be fetched
	*	@param paddingFunc A function returning the padding for specific coordinates (row, col)
	*	@return the value at row, col if row & col are inside the current matrix.
	*/
	def getForPadding(row:Int, col:Int, paddingFunc:(Int, Int) => Double):Double = {

		def isDefinedAtSubmatrix(r:Int, c:Int, subMatrixIdx:Int) = {
			def definedAtRow = r >= 0 && r < (if(isTransposeVar) realMappedCols(subMatrixIdx).array.length else realMappedRows(subMatrixIdx).array.length)
			def definedAtCol = c >= 0 && c < (if(!isTransposeVar) realMappedCols(subMatrixIdx).array.length else realMappedRows(subMatrixIdx).array.length)
			val isDefined = definedAtRow && definedAtCol

			isDefined
		}

		assert(realMappedRows.length == realMappedCols.length)

		var realRow = row
		var realCol = col
		var subMatrixIdx = realMappedRows.length-1
		while(subMatrixIdx > 0 && !isDefinedAtSubmatrix(realRow, realCol, subMatrixIdx)){
			val realRowOffset = realMappedRows(subMatrixIdx).range.min - realMappedRows(subMatrixIdx-1).range.min
			val realColOffset = realMappedCols(subMatrixIdx).range.min - realMappedCols(subMatrixIdx-1).range.min // add offset

			assert(realRowOffset >= 0)
			assert(realColOffset >= 0)
			if(isTransposeVar){
				realRow += realColOffset
				realCol += realRowOffset
			}
			else {
				realRow += realRowOffset
				realCol += realColOffset
			}

			subMatrixIdx -= 1

		}

		if(!isDefinedAtSubmatrix(realRow, realCol, subMatrixIdx)) {
			// At this point we know that realRow,realCol is outside of the original full matrix, so apply the given padding
			paddingFunc(realRow, realCol) // subMatrixIdx==0
		}
		else {
			// Go through the mapping for the correct submatrix
			val origRow = realMappedRows(subMatrixIdx).array(if(isTransposeVar) realCol else realRow)
			val origCol = realMappedCols(subMatrixIdx).array(if(isTransposeVar) realRow else realCol)
			// Get the value for those coordinates without going through the mapping again
			getRawValue(origRow,origCol)
		}
	}

	/**	Creates a submatrix containing only the rows in rowRange and cols in colRange
	*	This creates a view of the current matrix. Reinserting the submatrix will modify its state.
	*	Modifying the values in the matrix will NOT be reverted by unapplying the moves from the submatrix.
	*	Safe operations are changing the order of rows and columns.
	*	
	*	@param rowRange The rows to include in the submatrix
	*	@param colRange The columns to include in the submatrix
	*	@return an object to reinsert the submatrix back with or without applying the changes
	*/
	def subMatrix(rowRange:Range, colRange:Range) {
		/**
		* 	Order has importance, for the backup of the error
		*/
		getRows_=(Array(0), Some(rowRange))
		getCols_=(Array(0), Some(colRange))
		assert(realMappedRows.length == realMappedCols.length)
	}

	/**
	*	@see subMatrix
	*	@param rectangle The rectangle to include in the submatrix
	*/
	def subMatrix(rectangle:Rectangle){
		subMatrix(rectangle.toRowRange, rectangle.toColRange)
	}

	/**
	*	Reinserts all submatrices in reverse order and applies the changes
	*/
	def insertAllSubmatrix() {
		while(realMappedRows.length > 1 && realMappedCols.length > 1){
			insertSubmatrix()
		}
	}

	/**
	*	Reinserts the last submatrix without applying the changes
	*/
	def insertSubmatrixNoChange(){
		val StackMapping(rowRange, rowArray, rowError) = realMappedRows.remove(realMappedRows.length - 1)
		val StackMapping(colRange, colArray, colError) = realMappedCols.remove(realMappedCols.length - 1)
		theError = rowError
	}

	/** Inserts the last submatrix, applies the changes and
	*	updates the error to reflect those changes.
	*	Returns a ReverseMove that can be used to insert that same submatrix again,
	*	but without applying the changes
	*	
	*	@return a ReverseMove object to reverse the insertion of this submatrix 
	*/
	def insertSubmatrix():ReverseMove = {
		require(!isFullMatrix, "Can't insert a submatrix on a full matrix")
		assert(realMappedRows.length == realMappedCols.length)

		val StackMapping(rowRange, rowArray, rowError) = realMappedRows.remove(realMappedRows.length - 1)
		val StackMapping(colRange, colArray, colError) = realMappedCols.remove(realMappedCols.length - 1)

		// Create backup
		var rowBackup = realMappedRows.last
		rowBackup = StackMapping(rowBackup.range, rowBackup.array.clone, rowBackup.backupError)
		var colBackup = realMappedCols.last
		colBackup = StackMapping(colBackup.range, colBackup.array.clone, colBackup.backupError)


		// Apply the changes
		rowRange.zipWithIndex.foreach{ case(r, idx) =>
			mappedRows(r) = rowArray(idx)
		}
		colRange.zipWithIndex.foreach{ case(c, idx) =>
			mappedCols(c) = colArray(idx)
		}


		theError = None  // invalidate error

		new ReverseMove(){
			def reverseTheMove = {
				// remove current mapping
				// and reinsert old (unmodified) mapping
				realMappedRows(realMappedRows.length - 1) = rowBackup

				// remove current mapping
				// and reinsert old (unmodified) mapping
				realMappedCols(realMappedRows.length - 1) = colBackup

				// Restore the error of the matrix before it was transformed into a submatrix
				theError = rowError // use row error instead of colError, since rowError is the oldest error to have been computed
			}
			def moveName = s"submatrix"
		}
	}

	/** Updates the mapping of the rows if isRows, else columns
	*	
	*	@param isRows Modify the mapping of the rows or of the columns ?
	*	@param array The mapping to change
	*	@param rangeOpt if non empty, will create a submatrix with that range of rows 
	*/
	protected def getRowsOrCols(isRows:Boolean, array: Array[Int], rangeOpt:Option[Range] = None) {
		val arrayBackup = if(isRows) getRows else getCols
		val newMappedArray = if(rangeOpt.isEmpty) array
							else arrayBackup.slice(rangeOpt.get.min, rangeOpt.get.max+1)

		assert(newMappedArray.length <= arrayBackup.length) // Can only take sub-parts of the mapping

		val realMapped = if(isTransposeVar != isRows) realMappedRows else realMappedCols

		/*
		*	if rangeOpt is not empty, don't pop the stack, and an entry will be pushed on it
		* 	Otherwise, the top is poped, and a new entry is pushed (effectively replaces the top entry)
		*/
		val toPush = if(rangeOpt.isEmpty){
			val StackMapping(range, _, oldError) = realMapped.remove(realMapped.length - 1)

			StackMapping(range, newMappedArray, oldError) // keep old error
		}
		else {
			// no pop
			StackMapping(rangeOpt.get, newMappedArray, theError) // new error
		}
		realMapped += toPush

		if(rangeOpt.nonEmpty) theError = None  // invalidate error if took subset of rows/cols
	}


	/**	Updates original rows or columns(if transpose)
	*
	*	@param newRows An array containing the new ordering of the mapped rows for the current matrix (if isTranspose, will modify the cols)
	*	@param rowRange If non empty, creates a submatrix containing only the rows in rowRange. The first argument is then ignored
	*/
	private def getRows_=(newRows: Array[Int], rowRange:Option[Range] = None) {
		getRowsOrCols(true, newRows, rowRange)
	}

	/**	Updates original rows or columns(if transpose)
	*
	*	@param newRows An array containing the new ordering of the mapped rows for the current matrix (if isTranspose, will modify the cols)
	*/
	def getRows_=(newRows: Array[Int]) {
		getRowsOrCols(true, newRows, None)
	}

	/** Updates original columns or rows(if transpose)
	*	
	*	@param newCols An array containing the new ordering of the mapped columns for the current matrix (if isTranspose, will modify the rows)
	*	@param colRange If non empty, creates a submatrix containing only the cols in colRange. The first argument is then ignored
	*/
	private def getCols_=(newCols: Array[Int], colRange:Option[Range] = None) {
		getRowsOrCols(false, newCols, colRange)
	}

	/** Updates original columns or rows(if transpose)
	*	
	*	@param colRange If non empty, creates a submatrix containing only the cols in colRange. The first argument is then ignored
	*/
	def getCols_=(newCols: Array[Int]) {
		getRowsOrCols(false, newCols, None)
	}

	/**
	*	@return The mapping of the rows for the current matrix (or columns if transpose)
	*/
	def getRows:Array[Int] = if(isTransposeVar) mappedCols else mappedRows // Refers to original ROWS or COLS(if transpose)
	
	/**
	*	@return The mapping of the columns for the current matrix (or rows if transpose)
	*/
	def getCols:Array[Int] = if(isTransposeVar) mappedRows else mappedCols // Refers to original COLS or ROWS(if transpose)

	/**
	*	@return The number of rows of the current matrix (or columns if transpose)
	*/
	def rows:Int = getRows.length
	
	/**
	*	@return The number of columns of the current matrix (or rows if transpose)
	*/
	def cols:Int = getCols.length

	/**
	*	@param r The row index to check
	*	@return true if index r is in the range of row indices of the current matrix
	*/
	def isDefinedAtRow(r:Int):Boolean = r >= 0 && r < rows
	
	/**
	*	@param c The column index to check
	*	@return true if index c is in the range of column indices of the current matrix
	*/
	def isDefinedAtCol(c:Int):Boolean = c >= 0 && c < cols
	
	/**
	*	@param r The row index to check
	*	@param c The column index to check
	*	@return true if index c is in the range of column indices of the current matrix
	*/
	def isDefinedAt(r:Int, c:Int) = isDefinedAtRow(r) && isDefinedAtCol(c)

	/**
	*	@return an iterator on the rows and columns of this matrix
	*/
	def keysIterator = for(i <- (0 until rows).iterator; j <- (0 until cols).iterator) yield (i,j)

	/** Initialises the labels for this matrix and checks for integrity
	*/
	def init():Unit = {
		if(labelX == null) labelX = getCols.map(_.toString)
		if(labelY == null) labelY = getRows.map(_.toString)
		checkAll
	}

	/**	Checks For the integrity of the matrix
	*/
	def checkAll:Unit = {
	    require(initRows != 0, "rows are 0")
	    require(initCols != 0, "cols are 0")
		assert(mappedRows.nonEmpty)
		assert(mappedCols.nonEmpty)
		if(nBinaryMatrices > 1){
			// Ensure each coordinate has a single one for each category
			(0 until initRows).foreach{ r =>
				(0 until initCols).foreach{ c =>
					val v = (0 until nBinaryMatrices).foldLeft(0.0){case(acc, b) =>
						select(b)
						acc + apply(r,c)
					}
					assert(v==1.0, s"The coordinate ($r,$c) is not properly 1-hot encoded ! (sum equals $v)")
				}
			}
		}
		select(0)
	}

	/** Transposes this matrix
	*	
	*	@note The convolution is NOT updated when we transpose the matrix. This requires symetric kernels !
	*	@return this matrix
	*/
	def t:this.type = {
		isTransposeVar = !isTransposeVar
		this
	}

	override def toString:String = getMatrix.toString

	/**
	*	@return a string representing this matrix
	*/
	def allToString = {
		selectAll{idx =>
			toString
		}.zipWithIndex.map{case(stringMat, idx) =>
			s"Binary matrix $idx:\n$stringMat"
		}.mkString("\n")
	}

	override def equals(that: Any): Boolean =
	that match {
	  case that: MyMatrixLike => {
	  	this.rows == that.rows && this.cols == that.cols &&
	  	keysIterator.forall{case(r,c) => this(r,c) == that(r,c)}
	  }
	  case that: DenseMatrix[Double @unchecked] => {
	  	this.rows == that.rows && this.cols == that.cols &&
	  	keysIterator.forall{case(r,c) => this(r,c) == that(r,c)}
	  }
	  case _ => false
	}

	val similaritiesRow = Array.fill(rows)(-1)
	val similaritiesCol = Array.fill(cols)(-1)

	/** Convolutes the value at position (r,c)
	*	@param r the row index
	*	@param c the column index
	*/
	def convoluteSinglePoint(r:Int, c:Int):Double // Abstract

	/** Defines the popup to display regarding the X axis
	*/
	val tipX = new PartialFunction[Int, String]{
		def apply(c:Int) = labelX(getCols(c))
		def isDefinedAt(c:Int) = getCols.isDefinedAt(c)
	}

	/** Defines the popup to display regarding the Y axis
	*/
	val tipY = new PartialFunction[Int, String]{
		def apply(r:Int) = labelY(getRows(r))
		def isDefinedAt(r:Int) = getRows.isDefinedAt(r)
	}

	/** Writes this matrix into a file that can be loaded from LoadDataset
	*	
	*	@param filename The name of the backup
	*	@param replaceMapping If the mapping should be replaced from the mapping of the current matrix
	*/
	def writeToFile(filename:String, replaceMapping:(java.io.PrintWriter)=> Boolean=(p) => false) = {
		val fileNameFormated = filename.stripSuffix(".backup") + ".backup"
		// val m = getMatrix
		val wasTranspose = isTranspose
		if(isTranspose) this.t

		Utils.printToFile(new java.io.File(s"$fileNameFormated")){p =>
			assert(!isTranspose, "Can't save transposed matrix")

			p.println(this.desc)
			p.println(s"$rows $cols $nBinaryMatrices ${if(dontChangeColors) 1 else 0}")

			if(!replaceMapping(p)){
				p.println(realMappedRows.last.array.mkString(" "))
				p.println(realMappedCols.last.array.mkString(" "))
			}

			p.println(labelX.map(_.replaceAll(" ", "|%|")).mkString(" "))
			p.println(labelY.map(_.replaceAll(" ", "|%|")).mkString(" "))

			val selectedBackup = selected
			(0 until nBinaryMatrices).foreach{ b =>
				select(b)

				(0 until rows).foreach{r=>
					(0 until cols).foreach{c=>
						if(getRawValue(r,c)==1.0) {
							p.print(c)
							if(c!=cols-1) p.print(" ")
						}
					}
					p.println()
				}
				p.println("-----")
			}
			select(selectedBackup)
		}

		if(wasTranspose) this.t
	}


	/**
	*	@return (needs row labels, needs col labels)
	*/
	private def needsLabels:(Boolean, Boolean) = {
		if(desc.contains("football")) (true, true)
		else if(desc.contains("news")) (true, true)
		else if(desc.contains("mushrooms")) (true, false)
		else (false, false)
	}

	/**	Writes this matrix into a file that can be read by a read.table call from R
	*
	*	@param filename the name of the file
	*/
	def writeMatrixAsRInput(filename:String) = {
		val wasTranspose = isTranspose
		if(isTranspose) this.t
		
		val m = getMatrix

		Utils.printToFile(new java.io.File(filename + ".rformat")){p =>
			assert(!isTranspose, "Can't save transposed matrix")

			val needsLabelsCoding = needsLabels match {
				case (false, false) => 0
				case (false, true) => 1
				case (true, false) => 2
				case (true, true) => 3
			}

			// Start by printing all the col labels
			p.println(s"$needsLabelsCoding " + (0 until cols).map(c => tipX(c).replaceAll(" ","|%|")).mkString(" "))

			// Then print each row and with its label first
			(0 until rows).foreach{r => 
				val tip = tipY(r)
				p.print(tip.replaceAll(" ", "|%|"))
				p.print(" ")
				(0 until cols).foreach{c => 
					p.print(m(r, c))
					if (c != cols-1) p.print(" ")
				}
				p.println()
			}
		}

		if(wasTranspose) this.t
	}
}
