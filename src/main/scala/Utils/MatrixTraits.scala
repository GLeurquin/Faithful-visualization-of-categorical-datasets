package thesis.utils

import thesis.dataLoading._
import thesis.matrixTypes._

import breeze.linalg._
import breeze.plot._
import scala.collection.mutable.ArrayBuffer

import java.awt.Paint
import java.awt.Color

trait CanSetIterations {

	/** Sets the number of iterations to it
	*	
	*	@param it The number of iterations
	*/
	def setIterations(it:Int):Unit = ???
}

trait MatrixOrdering {

	/** Changes the ordering of the rows and columns of this matrix
	*
	*	@note m is changed
	*	@return the matrix where the rows and columns have been arranged to make it "nice" (not necessarily the same instance as m)
	*/
	def changeOrdering(m:MatrixMoves):MatrixMoves = ???
}

trait UsesKernelOptimization

trait MatrixHelpers {
	import scala.language.implicitConversions

	/**
	*	@param m The matrix on which labels may have to be added
	*	@return a optional tuple (matrix, ncat, iscat, tips)
	*	- matrix: The matrix with eventually more columns and rows, displaying the labels.
	*	these columns must be after m.cols, and rows after m.rows
	*	only one label may be displayed for a particular row or column. More than one
	*	row/col can be added to increase the visibility on the displayed matrix
	*	- ncat: the number of colors to be used to display the matrix
	*	- iscat: if the matrix should be displayed using colors
	*	if none is returned, the matrix will be displayed without any labels on the side
	*	- tips: a tip function for the labels to be merged with the original tips
	*/
	private def addLabels(m:MyMatrixLike):Option[(DenseMatrix[Double], Double, Boolean, PartialFunction[(Int,Int),String])] = {

		val colorOffset = if(m.isCat) m.nBinaryMatrices else 2
		val LABEL_PERC = 0.10

		lazy val moreRows = m.rows+(m.rows*LABEL_PERC).toInt
		lazy val moreCols = m.cols+(m.cols*LABEL_PERC).toInt

		if(m.desc.contains("football")){
			val matrix = m.getMatrix
			val newM = DenseMatrix.tabulate[Double](moreRows, moreCols){case (r,c) =>
				if(r < m.rows && c < m.cols) matrix(r,c)
				else if(r >= m.rows && c < m.cols) m.tipX(c).toInt + colorOffset
				else if(r < m.rows && c >= m.cols) m.tipY(r).toInt + colorOffset
				else 0
			}

			val myTips = new PartialFunction[(Int, Int), String] {
			    def apply(x: (Int,Int)) = {
			    	if(x._1 < m.rows && x._2 >= m.cols) m.tipY(x._1)
					else m.tipY(x._2)
			    }
			    def isDefinedAt(x: (Int,Int)) = (x._1 < m.rows && x._2 >= m.cols) || (x._1 >= m.rows && x._2 < m.cols)
			}

			Some((newM,14, true, myTips))
		}
		else if(m.desc.contains("news")){
			val nDistinctCat = m.labelY.distinct.size

			val matrix = m.getMatrix
			val newM = DenseMatrix.tabulate[Double](moreRows, moreCols){case (r,c) =>
				if(r < m.rows && c < m.cols) matrix(r,c)
				else if(r < m.rows && c >= m.cols) m.tipY(r).replaceAll(" ","").toInt + colorOffset - 1
				else if(r >= m.rows && c < m.cols) m.tipX(c).replaceAll(" ","").toInt + colorOffset - 1
				else 0 // Corner of the matrix, no meaning
			}

			val myTips = new PartialFunction[(Int, Int), String] {
			    def apply(x: (Int,Int)) = {
			    	if(x._1 < m.rows && x._2 >= m.cols) m.tipY(x._1)
					else m.tipY(x._2)
			    }
			    def isDefinedAt(x: (Int,Int)) = (x._1 < m.rows && x._2 >= m.cols) || (x._1 >= m.rows && x._2 < m.cols)
			}

			Some((newM,nDistinctCat + colorOffset, true, myTips))
		}
		else if(m.desc.contains("mushrooms")){
			val distinctCat = m.labelY.distinct.zipWithIndex.toMap

			val matrix = m.getMatrix
			val newM = DenseMatrix.tabulate[Double](m.rows, moreCols){case (r,c) =>
				if(r < m.rows && c < m.cols) matrix(r,c)
				else if(r < m.rows && c >= m.cols) distinctCat(m.tipY(r)) + colorOffset
				else 0 // Corner of the matrix, no meaning
			}

			val myTips = new PartialFunction[(Int, Int), String] {
			    def apply(x: (Int,Int)) = m.tipY(x._1)
			    def isDefinedAt(x: (Int,Int)) = x._1 < m.rows && x._2 >= m.cols
			}
			val nCat = distinctCat.size + colorOffset // Add one in case of binary matrix, because white and black are 2 colors
			Some((newM,nCat, true,myTips))
		}
		else None


	}


	private val whiteToBlack = GradientPaintScale(0.0, 1.0, PaintScale.WhiteToBlack)

	private class GradientPaintScaleBlackAndWhite[T](lower : T, upper : T, gradient : Array[Color])
	(implicit view : T=>Double)
	extends GradientPaintScale[T](lower, upper, gradient)(view) {
	  override def apply(value : T) : Paint = {
	    if (view(value).isNaN) {
	      PaintScale.nanPaint
	    } else {
	    	if(value >= 0.0 && value <= 1.0) whiteToBlack(value)
	    	else super.apply(value)
	    }
	  }
	}

	class CategoricalPaintScaleBlackAndWhite[T](lower : T, upper : T, gradient : Array[Color])
	(implicit view : T=>Double)
	extends CategoricalPaintScalePerso[T](lower, upper, gradient)(view) {
	  override def apply(value : T) : Paint = {
	    if (view(value).isNaN) {
	      PaintScale.nanPaint
	    } else {
	    	if(value >= 0.0 && value <= 1.0) whiteToBlack(value)
	    	else super.apply(value)
	    }
	  }
	}

	class CategoricalPaintScalePerso[T](lower : T, upper : T,gradient : Array[Color])
	(implicit view : T=>Double)
	extends GradientPaintScale[T](lower, upper, gradient)(view) {
	  override def apply(value : T) : Paint = {
	    if (view(value).isNaN) {
	      PaintScale.nanPaint
	    } else {
	    	gradient(view(value).toInt)
	    }
	  }
	}

	/** Opens a window with a visual representation of the matrix m
	*	
	*	@param m The matrix to view
	*	@param name The name to give to the window
	*	@return The figure that was created
	*/
	def visualize(m:MyMatrixLike, name:String):Figure = {
		val f = Figure(name)

		var myTips = new PartialFunction[(Int, Int), String] {
		    def apply(x: (Int,Int)) = s"(${m.tipX(x._2)}, ${m.tipY(x._1)})"
		    def isDefinedAt(x: (Int,Int)) = m.tipX.isDefinedAt(x._2) && m.tipY.isDefinedAt(x._1)
		}

		val (newM:DenseMatrix[Double], nCat:Double, isCat:Boolean, extTips:PartialFunction[(Int,Int),String]) = addLabels(m) match {
			case Some((newM:DenseMatrix[Double], nCat:Double, isCat:Boolean, extTips)) => (newM, nCat, isCat, extTips)
			case None => (m.getMatrix, m.nBinaryMatrices.toDouble, m.isCat, PartialFunction.empty[(Int,Int),String])
		}

		myTips = myTips.orElse(extTips)

		val colors = PaintScale.Category20.values.clone

		if(m.desc.startsWith("mushroomsCAT")){
			// swaps colors such that poisonous is red and edible is green
			val temp1 = colors(12)
			val temp2 = colors(13)
			colors(12) = colors(3)
			colors(13) = colors(2)
			colors(2) = temp2
			colors(3) = temp1
		}
		else if(m.desc.startsWith("mushrooms")){
			// swaps colors such that poisonous is red and edible is green
			val temp = colors(2)
			colors(2) = colors(3)
			colors(3) = temp
		}

		val scale = if(m.isCat && m.desc.startsWith("docGraph")){
							val grad = Array(new Color(87, 0, 0), Color.red, Color.orange, Color.white, Color.blue, Color.green, Color.gray, Color.magenta, Color.cyan)
							new CategoricalPaintScalePerso(0.0, nCat, grad)
						}
						else if(m.isCat && nCat <= 20) new CategoricalPaintScalePerso(0.0, nCat, colors)
						else if(m.isCat) GradientPaintScale(0.0, nCat, PaintScale.Rainbow)
						else if(nCat <= 20) new CategoricalPaintScaleBlackAndWhite(0.0, nCat, colors)
						else new GradientPaintScaleBlackAndWhite(0.0, nCat, PaintScale.Rainbow)

		if(isCat) f.subplot(0) += image(newM,scale, tips=myTips)
		else f.subplot(0) += image(newM, tips=myTips)
		f
	}


	/** Rescales this matrix with the given number of rows and cols,
	*	by applying a uniform kernel and then do linear sampling for binary matrices,
	*	and a majority vote for categorical matrices
	*	
	*	@param name The name of the figure
	*	@param height Window height
	*	@param width Window width
	*	@param rows Max number of rows that can still fully display for that height
	*	@param cols Max number of cols that can still fully display for that width
	*	@param oldFig an existing figure to use to draw the rescaled image. If none, a new figure is created
	*	@return The kernel size used to rescale the image, as well as the majority vote score (if applicable)
	*/
	def visualizeRescaled(	m:MyMatrixLike,
							name:String = "",
							height:Int = 1000,
							width:Int = 1000,
							rows:Int = 300,
							cols:Int = 300,
							padding:Padding = Padding.Boundary,
							oldFig:Option[Figure] = None
						):(String,Option[Double]) = {

		// Figure out the scaling factor for x and y:
		val newRows = rows.min(m.rows)
		val newCols = cols.min(m.cols)

		val scaleR = newRows.toDouble/m.rows // r' = scaleR * r
		val scaleC = newCols.toDouble/m.cols // c' = scaleC * c

		def toOrigR(r:Int) = (r/scaleR).toInt
		def toOrigC(c:Int) = (c/scaleC).toInt

		val Array(kernelRows, kernelCols) = Array(toOrigR(1).max(1), toOrigC(1).max(1)).map{
			case x if x%2 == 0 => x+1 // always have an odd sized kernel
			case x => x
		}

		val kernel = Kernels.ones(kernelRows, kernelCols)
		lazy val middleKernel = DenseMatrix.tabulate[Double](kernelRows, 1){case (i,j) =>
			kernel.matrix(i, kernel.cols/2)
		}

		val rescaleKernel = (if(m.dontChangeColors || !m.isCat) kernel.matrix else middleKernel)

		// sample the convoluted matrix:
		val matrix = m.getMatrix 

		val special = addLabels(m)
		val (newnewRows, newnewCols) = if(special.nonEmpty) (newRows + special.get._1.rows - m.rows, newCols + special.get._1.cols - m.cols)
									 else (newRows, newCols)

		val supCols = if(special.nonEmpty && m.cols < special.get._1.cols) special.get._1(::, m.cols) else null
		val supRows = if(special.nonEmpty && m.rows < special.get._1.rows) special.get._1(m.rows, ::).t else null

		lazy val cr = kernel.matrix.rows/2
		lazy val cc = kernel.matrix.cols/2
		lazy val normRow = (0 until kernel.matrix.rows).foldLeft(0.0){case (acc, kr) => acc + kernel.matrix(kr, cc)} // normalize
		lazy val normCol = (0 until kernel.matrix.cols).foldLeft(0.0){case (acc, kc) => acc + kernel.matrix(cc, kc)} // normalize

		/** Convolutes the point i of vector v using the middle row of the kernel if rows=true. Otherwise uses the middle column
		* 
		*	@param i index in v
		*	@param v The vector
		*	@param kernel The kernel to use
		*	@param rows true if v is representing row indices
		*/
		def getVectorMajorityVote(i:Int, v:DenseVector[Double], kernel:DenseMatrix[Double], rows:Boolean):Double = {
			if(rows){
				val half = kernel.rows/2
				(0 until kernel.rows).map{kr => 
					v((i+kr-half).max(0).min(v.size-1))
				}.groupBy(identity).mapValues(_.size).maxBy(_._2)._1
			}
			else{
				val half = kernel.cols/2
				(0 until kernel.cols).map{kc => 
					v((i+kc-half).max(0).min(v.size-1))
				}.groupBy(identity).mapValues(_.size).maxBy(_._2)._1
			}
		}


		var totalAgreement = 0.0 // Gives a score on how well the rescaled matrix represents the original matrix

		val labelRowsRescaled = new Array[String](newRows)
		val labelColsRescaled = new Array[String](newCols)

		val labelRowsRescaledLabels = new Array[String](newRows)
		val labelColsRescaledLabels = new Array[String](newCols)

		val scaledMatrix = DenseMatrix.tabulate[Double](newnewRows, newnewCols){case (r, c) =>

			val rowSquash = ((toOrigR(r)-rescaleKernel.rows/2).max(0) to (toOrigR(r)+rescaleKernel.rows/2).min(m.rows-1))
			val colSquash = ((toOrigC(c)-rescaleKernel.cols/2).max(0) to (toOrigC(c)+rescaleKernel.cols/2).min(m.cols-1))

			if(r < newRows && c < newCols){
				val newR = toOrigR(r)+rescaleKernel.rows/2
				val newC = toOrigC(c)+rescaleKernel.cols/2

				labelRowsRescaled(r) = rowSquash.map{x => m.tipY(x)}.mkString(",")
				labelColsRescaled(c) = colSquash.map{x => m.tipX(x)}.mkString(",")
				
				if(m.isCat) {
					/*
					*	Can't do a majority vote using a wide kernel if the value of the columns are not correlated.
					*	If not correlated, do a sampling instead, with a majority vote on a single column
					*/
					val (value, agreement) = matrix.majorityVote(newR, newC, rescaleKernel)
					totalAgreement += agreement
					value
				}
				else{
					val v = matrix.convoluteSinglePoint(newR, newC, rescaleKernel, padding)
					// Decide to round or not v here:
					v
					// math.round(v)
				}
			}
			else{
				// For the labels
				if(r < newRows && c >= newCols) {
					labelRowsRescaledLabels(r) = rowSquash.map{x => special.get._4(x,c)}.groupBy(identity).mapValues(_.size).mkString(",")
					getVectorMajorityVote(toOrigR(r), supCols, rescaleKernel, true)
				}
				else if(r >= newRows && c < newCols) {
					labelColsRescaledLabels(c) = colSquash.map{x => special.get._4(r,x)}.groupBy(identity).mapValues(_.size).mkString(",")
					getVectorMajorityVote(toOrigC(c), supRows, rescaleKernel, false)
				}
				else 0 // Corner has no meaning
			}
		}

		val myTips = new PartialFunction[(Int, Int), String] {
		    def apply(x: (Int,Int)) = {
		    	val (r,c) = x
		    	if(r < newRows && c < newCols) s"([${labelRowsRescaled(r)}], [${labelColsRescaled(c)}])"
		    	else if(r < newRows && c >= newCols) labelRowsRescaledLabels(r)
		    	else if(r >= newRows && c < newCols) labelColsRescaledLabels(c)
		    	else s"N/A"
		    }
		    def isDefinedAt(x: (Int,Int)) = x._1 < newnewRows && x._2 < newnewCols
		}

		val agreementScore = totalAgreement / (newRows*newCols) * 100
		// if(m.isCat) println(f"Rescaling score = $agreementScore%.2f")

		val isCat =  if(special.nonEmpty) special.get._3 else m.isCat

		val theImage = if(isCat) {
			val nCat = if(special.nonEmpty) special.get._2 else m.nBinaryMatrices.toDouble

			val colors = PaintScale.Category20.values.clone

			if(m.desc.startsWith("mushroomsCAT")){
				// swaps colors such that poisonous is red and edible is green
				val temp1 = colors(12)
				val temp2 = colors(13)
				colors(12) = colors(3)
				colors(13) = colors(2)
				colors(2) = temp2
				colors(3) = temp1
			}
			else if(m.desc.startsWith("mushrooms")){
				// swaps colors such that poisonous is red and edible is green
				val temp = colors(2)
				colors(2) = colors(3)
				colors(3) = temp
			}

			val scale = if(m.isCat && m.desc.startsWith("docGraph")){
							val grad = Array(new Color(87, 0, 0), Color.red, Color.orange, Color.white, Color.blue, Color.green, Color.gray, Color.magenta, Color.cyan)
							new CategoricalPaintScalePerso(0.0, nCat, grad)
						}
						else if(m.isCat && nCat <= 20) new CategoricalPaintScalePerso(0.0, nCat, colors)
						else if(m.isCat) GradientPaintScale(0.0, nCat, PaintScale.Rainbow)
						else if(nCat <= 20) new CategoricalPaintScaleBlackAndWhite(0.0, nCat, colors)
						else new GradientPaintScaleBlackAndWhite(0.0, nCat, PaintScale.Rainbow)
			image(scaledMatrix, scale, tips=myTips)
		}
		else image(scaledMatrix, tips=myTips)

		val f = oldFig match {
			case None => Figure(name)
			case Some(fig) => {
				fig.clear()
				fig
			}
		}

		// Create the figure
		f.width = width
		f.height = height
		f.subplot(0) += theImage
		return (s"${rescaleKernel.rows}x${rescaleKernel.cols}", (if(m.isCat) Some(agreementScore) else None))
	}

	implicit def denseMatrixToImprovedMatrix(m: DenseMatrix[Double]): ImprovedMatrix = new ImprovedMatrix(m)

	class ImprovedMatrix(m:DenseMatrix[Double]){
		import scala.collection.mutable.BitSet

		/**	If there are gaps in the numbers present in the matrix (ex: 1, 2, 3, 25),
		*	this prevents the creation of 25 matrices and only creates 4
		*	Assumes that the matrix is binary if it contains only 0s and 1s, where a
		*	0 represents the absence of a class, and a 1 its presence
		*	Assumes a categorical matrix otherwise. If it contains 0 and 1s,
		*	they will be considered as different categories
		*
		*	@return the number of categories (=1 if binary, >1 if categorical)
		*/
		def toSequentialCategories:Int = {
			var currentIdx = 0

			val categories = scala.collection.mutable.HashMap[Double, Int]()

			for(i <- 0 until m.rows; j <- 0 until m.cols){
				if(!categories.contains(m(i,j))) {
					categories(m(i,j)) = currentIdx
					currentIdx += 1
				}
			}

			if(categories.size == 2 && categories.contains(0.0) && categories.contains(1.0)) 1
			else {
				for(i <- 0 until m.rows; j <- 0 until m.cols){
					m(i,j) = categories(m(i,j))
				}
				categories.size
			}
		}

		/** Converts this matrix into an array of binary matrices
		*
		*	@return an array of bitsets representing the binary matrices
		*/
		def toOneHotEncoding:Array[BitSet] = {
			val nCategories = m.toSequentialCategories // Fill gaps in categories

			val bitSetArray = Array.fill(nCategories)(new BitSet(m.cols*m.rows))
			val bitSetArrayWithIdx = bitSetArray.zipWithIndex

			for( i <- 0 until m.rows; j <- 0 until m.cols) {
				bitSetArrayWithIdx.foreach{ case(bitSet,idx) =>
					val bit = 	if(nCategories > 1) {if(m(i,j) % nCategories == idx) 1.0 else 0.0}
								else m(i,j)
					if(bit == 1.0) bitSet += Utils.getHash(i, j, m.cols)
				}
			}

			bitSetArray
		}

		/**
		*	@param r Row index
		*	@return true if row r is inside this matrix
		*/
		def isDefinedAtRow(r:Int) = r >= 0 && r < m.rows

		/**
		*	@param c Column index
		*	@return true if column c is inside this matrix
		*/
		def isDefinedAtCol(c:Int) = c >= 0 && c < m.cols

		/**
		*	@param r Row index
		*	@param c Column index
		*	@return true if the coordinate (r,c) is inside this matrix
		*/
		def isDefinedAt(r:Int, c:Int) = isDefinedAtRow(r) && isDefinedAtCol(c)

		/**
		*	@return true if this matrix has the same number of rows & columns as the other matrix
		*/
		def sameDimAs(other:DenseMatrix[Double]) = m.rows == other.rows && m.cols == other.cols

		/**
		*	@return a random row
		*/
		def randomRow = Utils.rand.nextInt(m.rows)
		
		/**
		*	@return a random column
		*/
		def randomCol = Utils.rand.nextInt(m.cols)

		/**
		*	@param r Row index
		*	@param c Column index
		*	@param padding The padding to be used
		*	@return the padding for coordinate (r,c)
		*/
		def getPadding(r:Int, c:Int, padding:Padding):Double = {
          	padding match {
				case Padding.Cyclical => {
					// remember that r or c CAN be < 0
					val newR = if(r >= m.rows) r%m.rows else r
					val newC = if(c >= m.cols) c%m.cols else c
					m(newR,newC)
				}
                case Padding.Boundary => m( r.min(m.rows-1).max(0), c.min(m.cols-1).max(0) )
								case Padding.BoundaryAsymmetric => if(r < 0 || c > m.cols - 1) 0.0 else m( r.min(m.rows-1).max(0), c.min(m.cols-1).max(0) )
                case Padding.Zero => 0.0
                case Padding.ValueOpt(v: Double) => v
				case Padding.ZerosAndOnes =>{
					if(r < 0 || c < 0.0) 1.0 else 0.0
				}
                case op => require(false, "cannot handle Padding value " + op); 0.0
            }
		}

		/**
		*	@param r The row index
		*	@param c The column index
		*	@param kernel The kernel to use
		*	@param padding The padding to use
		* 	@return The convolution of point at position (r,c)
		*/
		def convoluteSinglePoint(r:Int, c:Int, kernel:DenseMatrix[Double], padding:Padding):Double = {
			val di = kernel.rows / 2 // floor division
			val dj = kernel.cols / 2 // floor division

			val mapped = kernel.mapPairs{case((kr,kc), kv) =>
				val theValue:Double = (r + kr - di, c + kc - dj) match {
					case (a,b) if isDefinedAt(a,b) => m(a, b)
					case (a,b) => getPadding(a,b, padding)
				}

				theValue*kv
			}
			sum(mapped)
		}

		/**
		*	@param kernel the kernel to use. Currenlty only uneven kernels supported.
		*	@param padding the policy to apply on the edges of the matrix. (Cyclical, Boundary, Zero or ValueOpt)
		*	@note post: the matrix m was not modified
		* 	@return the convolution of the matrix and the kernel
		*/
		def convoluteMatrixAndError(convolutionSettings:ConvolutionSettings): (DenseMatrix[Double],Double) = {
	        val kernel = convolutionSettings.kernel.matrix
	        val padding = convolutionSettings.padding
	        val errorType = convolutionSettings.errorType

	        require( m.cols * kernel.cols != 0, "data and kernel must have a non-empty number of columns DenseMatrix")
	        require( m.rows * kernel.rows != 0, "data and kernel must have a non-empty number of rows DenseMatrix")
	        require( kernel.rows % 2 == 1, "the kernel must have an odd number of rows" )
	        require( kernel.cols % 2 == 1, "the kernel must have an odd number of cols" )

			var error = 0.0

			val computeSingleError = Utils.getSingleErrorFunction(errorType)

	   		val convM = m.mapPairs{case((r,c), unused) =>

				val sum = convoluteSinglePoint(r,c, kernel, padding)
				val toAdd = if(m(r,c) != 0.0) computeSingleError(m(r,c), sum) else 0

				this.synchronized{	error += toAdd  }

				sum
	        }

			(convM,error)
     	} // End convolute

     	/**
		*	@return the most common value in the window defined by the kernel.
		*	the values of the kernel are not actually beeing used, only its size
     	*/
     	def majorityVote(r:Int, c:Int, kernel:DenseMatrix[Double]) = {
     		val cr = kernel.rows/2
     		val cc = kernel.cols/2
     		val counter = new scala.collection.mutable.HashMap[Double, Int]()
     		(0 until kernel.rows).foreach{kr =>
     			(0 until kernel.cols).foreach{kc =>
     				val ri = r + kr - cr
     				val ci = c + kc - cc
     				if(m.isDefinedAt(ri,ci)){
	     				val v = m(ri,ci)
	     				counter(v) = 1 + counter.getOrElseUpdate(v, 0)
     				}
     			}
     		}
     		val total = counter.map(_._2).sum

     		val vote = counter.maxBy(_._2)

     		val value = vote._1
     		val agreement = vote._2/total.toDouble

     		(value, agreement)
     	}

     	/**
     	*	@param convolutionSettings The parameters of the convolution
		*	@return The convoluted matrix
     	*/
		def convolute(convolutionSettings:ConvolutionSettings):DenseMatrix[Double] = {
			val r = convoluteMatrixAndError(convolutionSettings)
			r._1
		}

		/**
     	*	@param convolutionSettings The parameters of the convolution
		*	@return The error of the convoluted matrix
     	*/
		def computeError(convolutionSettings:ConvolutionSettings):Double = {
			val r = convoluteMatrixAndError(convolutionSettings)
			r._2
		}

	}



}
