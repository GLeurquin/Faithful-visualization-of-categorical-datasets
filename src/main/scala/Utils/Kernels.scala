package thesis.utils

import breeze.linalg._

/** Class that defines a kernel
*/
case class Kernel(var matrix:DenseMatrix[Double], name:String){
	require(matrix.rows%2==1, s"The number of rows of the kernel $name must be odd")
	require(matrix.cols%2==1, s"The number of cols of the kernel $name must be odd")
	override def toString:String = s"$name(${matrix.rows}x${matrix.cols})"

	/**
	*	@return the sum of the values in this kernel
	*/
	def sumKernel = sum(matrix)
	
	/**
	*	@return the number of rows of this kernel
	*/
	def rows = matrix.rows

	/**
	*	@return the number of columns of this kernel
	*/
	def cols = matrix.cols

	/**
	*	@return true if this kernel is equal to its transpose
	*/
	def isSymmetric:Boolean = matrix.equals(matrix.t)

	private var isTranspose_ = false

	/** Transposes this kernel
	*/
	def t:this.type = {
		matrix = matrix.t
		isTranspose_ = !isTranspose_
		this
	}

	/**
	*	@return true if this kernel is transposed
	*/
	def isTranspose = isTranspose_

	/**
	*	@return a copy of this kernel
	*/
	def copy:Kernel = Kernel(matrix.copy, name)
}

/** Different kernels that can be used for the convolution
*/
object Kernels {

	/**********************
	*		KERNELS
	**********************/

	def identity(size:Int):Kernel = identity(size, size)

	def identity(sizeRow:Int, sizeCol:Int) = Kernel({
		val matrix = DenseMatrix.zeros[Double](sizeRow, sizeCol)
		matrix(sizeRow/2,sizeCol/2) = 1.0
		matrix
	}, s"identity")


	def ones(sizeRow:Int, sizeCol:Int) = normalize(Kernel(
		DenseMatrix.ones[Double](sizeRow, sizeCol)
	, s"ones"))

	/**
	* 	Kernels with negative values are currently not supported!
	*/

	def almostIdentity = Kernel( DenseMatrix(
		(0.0, 1.0, 0.0),
		(0.0, 0.0, 0.0),
		(0.0, 0.0, 0.0)
	), "almostIdentity") // For debug only

	def almostIdentity5 = Kernel( DenseMatrix(
		(1.0, 2.0, 1.0, 2.0, 1.0),
		(2.0, 3.0, 1.0, 3.0, 2.0),
		(1.0, 1.0, 4.0, 1.0, 1.0),
		(2.0, 3.0, 1.0, 3.0, 2.0),
		(1.0, 2.0, 1.0, 2.0, 1.0)
	), "almostIdentity") // For debug only

	def edgeDetection1 = Kernel(DenseMatrix(
		(1.0, 	0.0, 	-1.0),
		(0.0, 	0.0, 	0.0),
		(-1.0,	0.0, 	1.0)
	), "edgeDetection1")

	def edgeDetection2 = Kernel(DenseMatrix(
		(0.0, 	1.0, 	0.0),
		(1.0, 	-4.0, 	1.0),
		(0.0,	1.0, 	0.0)
	), "edgeDetection2")

	def edgeDetection3 = Kernel(DenseMatrix(
		(-1.0, 	1.0, 	-1.0),
		(-1.0, 	8.0, 	-1.0),
		(-1.0,	-1.0, 	-1.0)
	), "edgeDetection3")

	def boxBlur = Kernel((1/9.0) :* DenseMatrix(
		(1.0, 	1.0, 	1.0),
		(1.0, 	1.0, 	1.0),
		(1.0,	1.0, 	1.0)
	), "boxBlur")

	def sharpen = Kernel(DenseMatrix(
		(0.0, 	-1.0, 	0.0),
		(-1.0, 	5.0, 	-1.0),
		(0.0,	-1.0, 	0.0)
	), "sharpen")

	def gaussianBlur = Kernel((1/16.0) :* DenseMatrix(
		(1.0, 2.0, 	1.0),
		(2.0, 4.0, 	2.0),
		(1.0,	2.0, 	1.0)
	), "gaussianBlur")

	def diagonal = Kernel((1/63.0) :* (DenseMatrix(
		(10.0, 	1.0, 	1.0),
		(10.0, 	10.0, 	1.0),
		(10.0, 	10.0, 	10.0)
	)), "diagonal")

	def bruteKernel = Kernel((1/8.0) :* DenseMatrix(
		(0.0, 1.0, 	0.0),
		(1.0, 4.0, 	1.0),
		(0.0,	1.0, 	0.0)
	), "bruteKernel")

	def cross = Kernel((1/6.0) :* DenseMatrix(
		(0.0, 1.0, 	0.0),
		(1.0, 2.0, 	1.0),
		(0.0,	1.0, 	0.0)
	), "crossKernel")

	def rectangleKernel = Kernel((1/4.0) :* DenseMatrix(
		(1.0),
		(2.0),
		(1.0)
	), "rectangleKernel")


	def gaussian(x:Int, y:Int, sigma:Int) = math.exp(-(x*x+y*y)/(2.0*sigma*sigma)) // no need to multiply by 1/2pi*s^2, since that's constant, and we'll normalize afterwards anyways

	def normalize(k:Kernel):Kernel = {
		Kernel(k.matrix :* (1.0)/sum(k.matrix), k.name)
	}

	/**
	*	@param size The number of rows & columns of the kernel
	*	@return a normalized exponential gaussian kernel (divide by 2 as we go further from center) of size size x size
	*/
	def gaussianBlurExp(size:Int):Kernel = gaussianBlurExp(size, size)
	
	/**
	*	@param sizeRows The number of rows of the kernel
	*	@param sizeCols The number of columns of the kernel
	*	@return a normalized exponential gaussian kernel (divide by 2 as we go further from center) of size sizeRows x sizeCols
	*/
	def gaussianBlurExp(sizeRows:Int, sizeCols:Int):Kernel = {
		normalize(gaussianBlurExpNotNormalized(sizeRows,sizeCols))
	}

	/**
	*	@param sizeRows The number of rows of the kernel
	*	@param sizeCols The number of columns of the kernel
	*	@return a non normalized exponential gaussian kernel (divide by 2 as we go further from center) of size sizeRows x sizeCols
	*/
	def gaussianBlurExpNotNormalized(sizeRows:Int, sizeCols:Int):Kernel = {
		require(sizeRows%2==1, "sizeRows must be odd")
		require(sizeCols%2==1, "sizeCols must be odd")

		val center:Int = sizeRows.max(sizeCols)/2
		val max = math.pow(2, 2 * center)

		val theKernel = DenseMatrix.tabulate[Double](sizeRows,sizeCols){case(i,j) =>
			val dist = Math.abs(i-sizeRows/2) + Math.abs(j-sizeCols/2)
			math.pow(2, 2 * center) / math.pow(2, dist)
		}

		Kernel(theKernel, s"gaussianBlurExp")
	}

	/**
	*	@param sizeRows The number of rows of the kernel
	*	@param sizeCols The number of columns of the kernel
	*	@return a normalized smoothed gaussian kernel (-1 as we go further from center)
	*/
	def gaussianBlurSmooth(sizeRows:Int, sizeCols:Int):Kernel = {
		normalize(gaussianBlurSmoothNotNormalized(sizeRows,sizeCols))
	}

	/**
	*	@param sizeRows The number of rows of the kernel
	*	@param sizeCols The number of columns of the kernel
	*	@return a non normalized smoothed gaussian kernel (-1 as we go further from center)
	*/
	def gaussianBlurSmoothNotNormalized(sizeRows:Int, sizeCols:Int):Kernel = {
		require(sizeRows%2==1, "sizeRows must be odd")
		require(sizeCols%2==1, "sizeCols must be odd")


		val centerR:Int = sizeRows/2
		val centerC:Int = sizeCols/2
		val center = centerR.max(centerC)

		val theKernel = DenseMatrix.tabulate[Double](sizeRows,sizeCols){case(i,j) =>
			center - Math.max(Math.abs(centerR-i), Math.abs(centerC-j)) + 1
		}

		Kernel(theKernel, s"gaussianBlurSmooth")
	}

	/**
	*	@param size The number of rows & columns of the kernel
	*	@return a real gaussian kernel
	*/
	def gaussianBlur(size:Int):Kernel = {
		require(size%2==1, "size must be odd")
		val center:Int = size/2

		val sigma = size/3 // set the mask size to be 3 times the standard deviation

		val scale = gaussian(-center, -center,sigma) // Set the scale to have the corner's weight equal to 1

		val theKernel = DenseMatrix.tabulate[Double](size,size){case(i,j) =>
			math.floor(gaussian(i-center,j-center,sigma)/scale)
		}

		val scaledKernel = theKernel
		Kernel(scaledKernel, s"gaussianBlur")
	}


	/**
	*	@param size The number of rows & columns of the kernel
	* 	@return a normalized cross kernel of size size x size
	*/
	def cross(size:Int):Kernel = cross(size, size)

	/**
	*	@param sizeRows The number of rows of the kernel
	*	@param sizeCols The number of columns of the kernel
	* 	@return a normalized cross kernel of size sizeRows x sizeCols
	*/
	def cross(sizeRows:Int, sizeCols:Int):Kernel = {
		normalize(crossNotNormalized(sizeRows, sizeCols))
	}


	/**
	*	@param sizeRows The number of rows of the kernel
	*	@param sizeCols The number of columns of the kernel
	* 	@return a cross kernel with sum = 1 where the weight diminishes exponentially to the borders
	*/
	def crossNotNormalized(sizeRows:Int, sizeCols:Int):Kernel = {
		require(sizeRows%2==1, "sizeRows must be odd")
		require(sizeCols%2==1, "sizeCols must be odd")

		val center:Int = sizeRows.max(sizeCols)/2

		val theKernel = DenseMatrix.tabulate[Double](sizeRows,sizeCols){case(i,j) =>
			if(i == sizeRows/2)	Math.pow(2,center-Math.abs(j-sizeCols/2))
			else if (j == sizeCols/2) Math.pow(2,center-Math.abs(i-sizeRows/2))
			else 0.0
		}
		Kernel(theKernel, s"cross")
	}
}
