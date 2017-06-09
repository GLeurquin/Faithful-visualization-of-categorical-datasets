package thesis.orderings
import thesis.matrixTypes._
import thesis.utils._
import thesis.rectangles._

/**	Starts from lineStep rows, reorders them and then iteratively adds lineStep rows 
*	followed by the ordering until the full matrix is selected
*	
*	@param isCanceled: returns true to stop the deph search
*	@param ordering: the ordering to use to order the matrix at each step
*	@param lineStep: the number of lines to add at each step
*/
class DepthSearch(isCanceled: ()=>Boolean, ordering:MatrixOrdering, lineStep:Int = 100) extends MatrixOrdering with MatrixHelpers with CanSetIterations{	

	/**
	*	@inheritdoc
	*/
	override def changeOrdering(m: MatrixMoves) = {

		class BoundaryIterator() extends Iterator[Rectangle]{
			private val initRow = m.randomRow
			private var currentBoundary = (initRow to initRow)

			private def canExtendUp = m.isDefinedAtRow(currentBoundary.min-1) // step must be 1 here
			private def canExtendDown = m.isDefinedAtRow(currentBoundary.max+1) // step must be 1 here

			def hasNext = canExtendUp || canExtendDown

			def next() = {
				currentBoundary = (canExtendUp, canExtendDown) match {
					case (true, true) => {
						// extend up or down randomly
						if(Utils.rand.nextDouble < 0.5) ((currentBoundary.min-lineStep).max(0) to currentBoundary.max) // Up
						else (currentBoundary.min to (currentBoundary.max+lineStep).min(m.rows-1)) // Down
					}
					case (true, false) => ((currentBoundary.min-lineStep).max(0) to currentBoundary.max) // Up
					case (false, true) => (currentBoundary.min to (currentBoundary.max+lineStep).min(m.rows-1)) // Down
					case (false, false) => assert(false, "No more boundaries to iterate over"); (0 to 0)
				}

				val topLeft = Coord(currentBoundary.min, 0)
				val bottomRight = Coord(currentBoundary.max, m.cols-1)
				Rectangle(topLeft,bottomRight)
			}
		}

		val iterator = new BoundaryIterator()

		while(iterator.hasNext && !isCanceled()){
			val rectangle = iterator.next()
			m.subMatrix(rectangle)

			ordering.changeOrdering(m)

			m.insertSubmatrix
			println(rectangle)
		}

		m
	}

}
