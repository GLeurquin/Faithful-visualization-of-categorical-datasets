package thesis.orderings
import thesis.matrixTypes._
import thesis.utils._
import thesis.UI._

import breeze.linalg._
import breeze.plot._

class BandedBarycentric(MAXIT: Int,
					progressUpdate:(Double, Double)=>Unit=(d:Double,e:Double) => Unit,
					isCanceled: ()=>Boolean = ()=>false,
					showProgress:Boolean = false)
extends BandedOrdering(MAXIT, progressUpdate, isCanceled, showProgress) with MatrixOrdering {

	override def toString:String = {
		val desc = s"Barycentric"
		f"$desc%-35s " + super.toString
	}

	/**
	*	@return The average position of 1s (or other value) in a row
	*/
	private def barycenter(v:DenseVector[Double]):Double = {
		sum(v.mapPairs{case(index, value) => index * value}) / sum(v).toDouble
	}

	/**
	*	@note post: m is unchanged
	*	@return An ordering where the index represents the previous row or column, and its value represents its position in the ordered matrix
	*/
	private def barycentric(m:DenseMatrix[Double]):Array[Int] = {
		// Compute the ordering of the rows according to the barycenter
		(0 until m.rows).map{ r =>
			barycenter(m(r, ::).t)
		}.zipWithIndex
		.sortBy{case(bary, rowIndex) => bary}
		.map{case(bary, rowIndex) => rowIndex}
		.toArray
	}

	/**
	*	@inheritdoc
	*/
	override def changeOrdering(m:MatrixMoves) = {
		if(m.isCat) throw new IllegalArgumentException("Barycentric ordering only works for binary data")
		alternate(m,barycentric)
	}
}


class BandedUnidirectional(MAXIT: Int,
					progressUpdate:(Double, Double)=>Unit=(d:Double,e:Double) => Unit,
					isCanceled: ()=>Boolean = ()=>false,
					showProgress:Boolean = false)
extends BandedOrdering(MAXIT, progressUpdate, isCanceled, showProgress) with MatrixOrdering {
	override def toString:String = {
		val desc = s"Unidirectional"
		f"$desc%-35s " + super.toString
	}
	/** The fixed permutation algorithm for MBA
	*
	*	@note post: m is unchanged
	*	@return An ordering where the index represents the previous row or column, and its value represents its position in the ordered matrix
	*/
	private def unidirectionalFixedPermutation(m:DenseMatrix[Double]):Array[Int] = {

		val intervals = mapIntervals(m.t)
		assume(intervals.length == m.rows ,"The length of the intervals should be equal to the number of rows in m")

		// Compute the optimal extension
		(0 until intervals.length).par.map{ r =>
			val theInterval = intervals(r)

			if(theInterval.isEmpty) (theInterval, r) // return the original interval
			else{
				val c = intervals.zipWithIndex.filter{case (interval, index) =>
					(index != r) && theInterval.isStrictlyIncludedIn(interval) && !interval.isEmpty
				}.map{case (interval, index) => interval}

				if(c.isEmpty) (theInterval, r) // return the original interval
				else{
					val optionA = Interval(c.map{case Interval(f,l) => f}.min, theInterval.last) // left hand side extension
					val optionB = Interval(theInterval.first, c.map{case Interval(f,l) => l}.max) // right hand side extension

					// Both left and right hand sides, with a combination of two superintervals
					val optionC = c.map{case Interval(f,l) =>
						val maxOne = c.filter{case Interval(first,last) => first <= f} // Had to put a <= here instead of <, otherwise weird things happen
						.map{case Interval(first,last) => last}
						.max
						Interval(f, maxOne)
					}

					// The best interval
					val theBestInterval = Array(Array(optionA), Array(optionB), optionC).flatMap(x => x).minBy(x => theInterval.numberOfTransformations(x))
					(theBestInterval,r)
				}
			}
		}
		.seq.sortBy{case (interval,rowNumber) => interval}
		.map{case(interval, rowNumber) => rowNumber}
		.toArray
	}

	/**
	*	@inheritdoc
	*/
	override def changeOrdering(m:MatrixMoves) = {
		if(m.isCat) throw new IllegalArgumentException("Unidirectional ordering only works for binary data")
		alternate(m,unidirectionalFixedPermutation)
	}
}


class BandedBidirectional(MAXIT: Int,
					weight: =>(Double)=>Double = (x:Double)=>if(x==1.0) 1.0 else -1.0,
					progressUpdate:(Double, Double)=>Unit=(d:Double,e:Double) => Unit,
					isCanceled: ()=>Boolean = ()=>false,
					showProgress:Boolean = false)
extends BandedOrdering(MAXIT, progressUpdate, isCanceled, showProgress) with MatrixOrdering {
	override def toString:String = {
		val desc = s"Bidirectional"
		f"$desc%-35s " + super.toString
	}

	/**
	* 	@param w The weight function, for example a function which makes each 1 in m become 1 in w, and 0 in m become -1 in w
	*	@note post: m is unchanged
	*	@return An ordering where the index represents the previous row or column, and its value represents its position in the ordered matrix
	*/
	private def bidirectionalFixedPermutation(mat:DenseMatrix[Double]):Array[Int] = {
		// Convert the matrix to consecutive ones
		val intervals = (0 until mat.rows).map{ i =>
			val weightMatrix = mat(i,::).t.mapValues(weight)
			val (interval, sum) = Utils.maximumConsecutiveSubarray(weightMatrix)
			interval
		}.toArray

		// Resolve Sperner conflicts
		for(
			i <- 0 until mat.rows;
			j <- 0 until mat.rows
			if i != j
			if intervals(i) isIncludedIn intervals(j)
			if !intervals(i).isEmpty
		) {
			val mi = intervals(i)

			val (left, right) = intervals(j) diff mi // compute mj\mi
			val wA = (mat(j,::) - mat(i,::)).t.map{i => if(i==1.0) 1.0 else -1.0 }
			val (intervalAB, sumTemp) = Utils.maximumConsecutiveSubarray(wA)
			if(intervalAB == intervals(j)) intervals(i) = Interval(-1,-1)
			else if(left.size < right.size)	intervals(i) = Interval(left.first, mi.last) // extend mi with the left interval
			else 	intervals(i) = Interval(mi.first, right.last) // extend mi with the right interval
		}

		// Make the ordering
		intervals.zipWithIndex
		.sortBy{case (interval,rowNumber) => interval}
		.map{case(interval, rowNumber) => rowNumber}
		.toArray
	}

	/**
	*	@inheritdoc
	*/
	override def changeOrdering(m:MatrixMoves) = {
		if(m.isCat) throw new IllegalArgumentException("Bidirectional ordering only works for binary data")
		alternate(m,bidirectionalFixedPermutation)
	}
}

/**
*	@param maxIt The maximum number of iterations
*	@param progressUpdate A function that takes the iteration percentage and the error, used for display
*	@param isCanceled() returns true if this ordering should stop early
*	@param showProgress If true, displays the rescaled image progress every 20 iterations
*/
abstract class BandedOrdering(MAXIT: Int,
					progressUpdate:(Double, Double)=>Unit=(d:Double,e:Double) => Unit,
					isCanceled: ()=>Boolean = ()=>false,
					showProgress:Boolean = false) extends MatrixHelpers {
	private var it = 0.0

	require(MAXIT > 0, "the maximum number of iterations must be greater than 0")

	override def toString:String = s"(${MAXIT})"

	/**
	*	 @return The index of the first one in the vector, or -1 if no one was found
	*/
	protected def findLastOne(v:DenseVector[Double]):Int = {
		var i = v.size-1
		while(i >= 0){
			if(v(i) == 1) return i
			i -= 1
		}
		-1
	}

	/**
	*	 @return The index of the first one in the vector, or -1 if no one was found
	*/
	protected def findFirstOne(v:DenseVector[Double]):Int = {
		var i = 0
		while(i < v.size){
			if(v(i) == 1) return i
			i += 1
		}
		-1
	}

	/** Converts the matrix in an array representing the minimum and maximum index of a ONE, for each row
	*	
	*	@note post: m is unchanged
	*/
	protected def mapIntervals(m:DenseMatrix[Double]):Array[Interval] = {
		(0 until m.rows).par.map{ r =>
			val theRow = m(r,::).t
			val firstOne = findFirstOne(theRow)
			if(firstOne < 0){
				Interval(-1, -1)
			}
			else{
				Interval(firstOne, findLastOne(theRow))
			}
		}.seq.toArray
	}


	def isSameOrdering(ordering:Array[Int]):Boolean = ordering.sameElements(Array.range(0, ordering.length))
	/**
	*	@note post: m is changed
	*	@param m The matrix in which we'd like to find banded patterns
	*	@return A matrix reordered to show a banded pattern
	*/
	protected def alternate(m:MatrixMoves, orderingFunction: => (DenseMatrix[Double])=>Array[Int]):MatrixMoves = {
		it = 1.0 // number of iterations, instance variable

		val figure = if(showProgress) Figure("Progress") else null
		val initError = m.getError

		while(it <= MAXIT && !isCanceled()){
			val perm = orderingFunction(m.getMatrix)
			m.permute(perm)
			m.t

			if(showProgress && it % 20 == 0) visualizeRescaled(m, "Progress", oldFig=Some(figure))
			progressUpdate(it/MAXIT, initError) // don't change the error incrementally, because it is costly
			it += 1 // Increase number of iterations
		}

		if(m.isTranspose) m.t
		else m
	}
}
