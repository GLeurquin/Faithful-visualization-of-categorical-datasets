package thesis.orderings
import thesis.matrixTypes._
import thesis.utils._

import breeze.linalg._

class NestedOrdering extends MatrixOrdering{
	override def toString:String = "Nested"

	/**
	* 	@param c vector containing the sum of each column
	* 	@return a vector containing the sum of each row
	*/
	private def conjugate(c:Array[Int]) : Array[Int] = {

		val m = c.reduceLeft{ _ max _} // max number of ones
		var x = 1
		var y = m
		val s = Array.fill(m){0}

		while(y>=1){
			if(x>c.size || y>c(x-1)){
				s(y-1)=x-1
				y-=1
			}
			else{
				x+=1
			}
		}
		s
	}

	/**
	* 	@param r vector containing the sum of each row
	* 	@param c vector containing the sum of each column
	* 	@return true if the matrix has a nested structure
	*/
	private def isNested(r:Array[Int], c:Array[Int]):Boolean = {
		val s = conjugate(c.sortWith{case(a,b) => a > b})
		if(s.deep==r.deep) true
		else false
	}

	/**
	*	@inheritdoc
	*/
	override def changeOrdering(m: MatrixMoves) = {
		if(m.isCat) throw new IllegalArgumentException("Nested ordering only works for binary data")
		// sort the rows by increasing numbers of ones
		val orderRows = argsort{
			sum{
				m.getMatrix(*,::)
			}
		}.reverse.toArray

		m.permute(orderRows)
		// sort the cols by increasing numbers of ones
		val orderCols = argsort{
			sum{
				m.getMatrix(::,*)
			}.t
		}.reverse.toArray

		m.t.permute(orderCols)
		m.t
	}
}
