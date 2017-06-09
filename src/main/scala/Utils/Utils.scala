package thesis.utils

import thesis.matrixTypes._

import breeze.linalg._
import java.io.File
import scala.io.Source
import breeze.plot._

object Utils extends MatrixHelpers{

	val SEED = 54544726
	val rand = new scala.util.Random(SEED)

	/**	Distance function for the TSP
	*	
	*	@param m The binary matrix on which to compute the distance
	*	@param i The row coordinate
	*	@param j The column coordinate
	*/
	val distanceBinary = (m: DenseMatrix[Double], i: Int, j: Int) => Distance.sum_diff(m,i,j)
	
	/** Distance function for the TSP
	*
	*	@param m The categorical matrix on which to compute the distance
	*	@param i The row coordinate
	*	@param j The column coordinate
	*/
	val distanceCat = (m: DenseMatrix[Double], i: Int, j: Int) => Distance.sum_diffCat4(m,i,j)

	/** Class representing a result and the time it took to get it 
	*/
	case class TimeResult[R](result:R, time:Long)

	/** Helper function that can write to a new file or append text to an existing file
	*
	*	@param f The file on which to write
	*	@param append Should it append on the file or overwite it ?
	*/
	def printToFile(f: java.io.File, append:Boolean = false)(op: java.io.PrintWriter => Unit) {
		val foutput = new java.io.FileOutputStream(f,append)
		val p = new java.io.PrintWriter(foutput)
		try { op(p) } finally {
			p.close()
			foutput.close()
		}
	}

	/**
	*	@param dir The directory for which the files should be listed
	*	@return A list of files in the given directory
	*/
	def getListOfFiles(dir: String):List[File] = {
	    val d = new File(dir)
	    if (d.exists && d.isDirectory) {
	        d.listFiles.filter(_.isFile).toList
	    } else {
	        List[File]()
	    }
	}

	/** Helper function that can time a block of code and return its result 
	*
	*	@param block The block of code to time and execute
	*	@return a TimeResult object containing the result of block, and its execution time
	*/
	def time[R](block: => R): TimeResult[R] = {
	    val t0 = System.currentTimeMillis()
	    val result = block    // call-by-name
	    val t1 = System.currentTimeMillis()
	    val elapsedTime = t1 - t0 // ms
	    TimeResult(result, elapsedTime)
	}

	/**
	*	@param r The row
	*	@param c The column
	*	@param cols The number of columns in the dataset
	*	@return The index of a coordinate (r,c) as if the matrix was a single vector where the rows were put one after each other
	*/
	def getHash(r:Int, c:Int, cols:Int) = {
		r*cols + c
	}

	/**	Groups the values in range by batchSize
	*
	*	@param batchSize The batch size
	*	@param range The range of indices to group
	*	@return a list of indices from range.min to range.max by batchSize, including range.max
	*/
	def makeBatches(range:Range, batchSize:Int):IndexedSeq[Int] = {
		require(batchSize > 0, "The batchSize can't be less than 0 !")
		val temp = (range.min to range.max by batchSize)
		if(!temp.contains(range.max)) temp :+ range.max
		else temp
	}

	/**	Finds the consecutive subarray in v which has the largest sum, in linear time
	*
	* 	@return the minimum and maximum indexes of the subarray, inclusive, as well as the corresponding sum
	*	@example 
	*	v = -2, -3, 4, -1, -2, 1, 5, -3
	* 	max sum = 4 + (-1) + (-2) + 1 + 5 = 7
	*	--> returns interval(2,6) and the sum
	*/
	def maximumConsecutiveSubarray(v:DenseVector[Double]):(Interval, Double) = {
		var max_so_far = 0.0
		var max_ending_here = 0.0

		var minIndex = 0 // Minimum index of the currently considered subarray
		var bestInterval = Interval(0, 0)

		v.foreachPair{case(index, value) =>
			// reset the minIndex when the max_ending_here is equal to 0 and the current value is positive
			if(value > 0 && max_ending_here == 0) minIndex = index

			max_ending_here += value // Add current value to current sum
			max_ending_here = max_ending_here.max(0) // Remove negative sums

			if(max_so_far < max_ending_here){
				max_so_far = max_ending_here
				bestInterval = Interval(minIndex, index)
			}
		}

		(bestInterval, max_so_far)
	}

	/**
	*	@param errorType The error type to use
	*	@return a function that computes the error of type errorType from the value of point and its convolution
	*/
	def getSingleErrorFunction(errorType:ErrorType):(Double, Double) => Double = {
		errorType match {
			case ErrorType.Abs =>
				(value:Double, conv:Double) => {
					val diff = value - conv
					Math.abs(diff)
				}
			case ErrorType.Square =>
				(value:Double, conv:Double) => {
					val diff = value - conv
					diff :* diff
				}
		}
	}

	/** Represents a rescaled dataset
	*
	*	@param fig The figure representing the rescaled dataset
	*	@param kernelSize The size of the kernel that was used for the rescaling
	*	@param agreementScore Has a value only for categorical datasets. Represents the score of the majority vote
	*/
	case class RescaledResult(fig:Figure, kernelSize:String, agreementScore:Option[Double])

	/**
	*	@param dataset The matrix to rescale
	*	@return a RescaledResult with the rescaled figure
	*/
	def getRescaledFigure(dataset:MatrixMoves) = {
		val figure = new Figure("hi")
		figure.visible=false
		val (kernelSize, agreementScore) = visualizeRescaled(     dataset,
		                                       height = 1000,
		                                       width = 1000,
		                                       rows = 300,
		                                       cols = 300,
		                                       padding = Padding.ZerosAndOnes,
		                                       oldFig = Some(figure)
		                                 )
		assert(figure!=null)
		RescaledResult(figure, kernelSize, agreementScore)
	}

	/**	Transforms the filename into a valid filename
	*
	*	@param f the filename to sanitize
	*	@return a valid filename
	*/
	def sanitizeFilename(f:String) = {
		var x = f
		x = x.replaceAllLiterally(".", ",") // can't have dots in filenames
		x = x.replaceAllLiterally(" ", "") // can't have spaces in filenames
		x = x.replaceAllLiterally(",", "") // can't have commas in filenames
		x
	}

	/**
	*	@param data The matrix to save to a rescaled image and to a .backup
	*	@param filenamePrefix The prefix to add to the filename of the image and of the backup
	*/
	def save(data:MatrixMoves, filenamePrefix:String) = {
		val RescaledResult(fig, scale, agreementScore) = getRescaledFigure(data)
		var figName = s"$filenamePrefix-Scale($scale)"
		if(agreementScore.nonEmpty) figName += f"-MajVote(${agreementScore.get}%.0f)"
		figName = sanitizeFilename(figName)
		assert(fig!=null)
		fig.saveas(s"$figName.png")
		println(s"Done $figName")

		// Save as backup file
		data.writeToFile(figName)

		figName
	}

}
