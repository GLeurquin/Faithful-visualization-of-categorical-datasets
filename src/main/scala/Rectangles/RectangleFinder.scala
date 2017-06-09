package thesis.rectangles

import thesis.utils._
import thesis.matrixTypes._

import scala.collection.mutable.ArrayBuffer

/** Class that represents a boundary
*/
case class Boundary(up: Int, down: Int){
	var col = -1
	def setCol(colNew: Int) = col = colNew

	def toStringFull = s"Boundary $up $down $col"

	def isIncludedIn(other: Boundary):Boolean = {
		other.up <= this.up && this.down <= other.down
	}
}

/** Object used to find two rectangles in the matrix
* 	such that the intersection of their columns is not empty.
*/
object RectangleFinder{

	private val MIN_AREA = 15 // minimal area that the rectangles must have

	/** Finds the first random point
	*	
	*	@param m The matrix in which to search the first point
	*	@return The coordinate and if it succeeded to find a point fast enough
	*/
	private def findP1(m: MatrixMoves) : (Coord, Boolean) = {

		val MAXTRIES = 100

		//selects a matrix
		val selectedMatrixP1 = Utils.rand.nextInt(m.nBinaryMatrices)
		m.select(selectedMatrixP1)
		var p1 = Coord(m.randomRow, m.randomCol) // random point
		var tries_0 = 0

		// try until we find a one
		while(m(p1.row, p1.col) == 0.0 && tries_0 < MAXTRIES){
			p1 = Coord(m.randomRow, m.randomCol)
			tries_0 += 1
		}

		(p1, tries_0 < MAXTRIES)
	}

	/**
	* 	@return If numMatrixP1 != -1 returns numMatrixP1
	* 	Otherwise, return the number of the matrix in the one hot encoding
	* 	such that m(p2.row, p2.col) = 1. If the matrix is binary and that m(p2.row, p2.col) = 0
	* 	returns -1
	*/
	private def newNumMatrix(m: MatrixMoves, p2: Coord, numMatrixP1: Int) : Int = {

		if(numMatrixP1 != -1) return numMatrixP1

		var res = -1

		m.selectAll{(idx) =>
			if(m(p2.row, p2.col) != 0) res = idx
		}

		res
	}

	/** Uses the first random point p1, the first rectangle r1 and the number of the matrix
	* 	containing them to find a second rectangle R2. R2 must be such that the intersection of its columns
	* 	with the ones of R1 is not empty
	*
	*	@param m The matrix in which to search the second point
	*	@param p1 The first coordinate
	*	@param r1 The first rectangle
	*/
	private def findR2(m: MatrixMoves, p1: Coord, r1: Rectangle, numMatrixP1: Int) = {

		val MAXTRIES = 100
		var p2 = Coord(m.randomRow, p1.col) // random point on the same column as P1
		var numMatrix = newNumMatrix(m, p2, numMatrixP1) // finds the matrix such that p2 is equal to one

		// try until p2 is equal to one and is not contained in r1
		var tries = 0
		while(numMatrix == -1 || (r1.contains(p2) && tries < MAXTRIES)  ){
			p2 = Coord(m.randomRow, p1.col)
			numMatrix = newNumMatrix(m, p2, numMatrixP1)
			tries += 1
		}

		// selects the right matrix and finds the rectangle containing p2
		m.select(numMatrix)
		var r2 = new MaxRectangleBuilder(m, p2, (r:Rectangle) =>
		  Math.sqrt(r.height) * r.intervalOverlapCols(r1) //r.area *	Math.pow(1.1, - 1 * Math.abs(r.width - r1.width)) // * Math.pow(1.5, r.intervalOverlapCols(r1))
			, 0).findMaxRectangle
		// set the color of rectangle r2 (used only for categorical matrices)
		r2.setColor(numMatrix)
		(r2, r2.area >= MIN_AREA)
	}

	/**
	*	@param m The matrix in which the rectangle should be searched
	*	@return Two rectangles that are roughly on the same row. They may or may not overlap
	*/
	def getSuitableRectangles(m:MatrixMoves):Array[Rectangle] = {
		val MAX_FIND = 100
		val isTranspose = m.isTranspose

		// find suitable p1

		var res = findP1(m)
		var p1 = res._1
		var foundPoint = res._2
		var selectedMatrixP1 = m.selected

		while(!foundPoint){
			res = findP1(m)
			p1 = res._1
			foundPoint = res._2
		}

		/************
  			find r1
		************/

		var r1 = new MaxRectangleBuilder(m, p1, (r:Rectangle) => r.width * Math.sqrt(r.height), 0).findMaxRectangle
		var bigEnough = r1.area >= MIN_AREA

		var tries_1 = 0

		// try until r1 is big enough
		while(!bigEnough && tries_1 < MAX_FIND){
			p1 = Coord(m.randomRow, m.randomCol)
			while(m(p1.row, p1.col) == 0){
				p1 = Coord(m.randomRow, m.randomCol)
			}
			r1 = new MaxRectangleBuilder(m, p1, (r:Rectangle) => r.width * Math.sqrt(r.height), 0).findMaxRectangle
			bigEnough = r1.area >= MIN_AREA
			tries_1 += 1
		}

  		/************
  			find r2
		************/
		// set the color of r1 (used only for categorical matrices)
		r1.setColor(selectedMatrixP1)

		// if the matrix is categorical there are two cases:
		// 1) If the matrix is not transposed then R2 must have the same color as R1
		//		Thus we set numMatrixP2 to selectedMatrixP1
		// 2) R2 can have a different color and we set numMatrixP2 to -1 so any rectangle will be okay
		val numMatrixP1 = if(isTranspose && !m.dontChangeColors) -1 else selectedMatrixP1

		var res2 = findR2(m, p1, r1, numMatrixP1)

		bigEnough = res2._2
		var r2 = res2._1

		// retries until the rectangle is big enough
		var tries_2 = 0
		while(!bigEnough && tries_2 < MAX_FIND){
			res2 = findR2(m, p1, r1, numMatrixP1)
	 		bigEnough = res2._2
	 		r2 = res2._1
	 		tries_2 += 1
		}
		Array(r1,r2)
	}

}


/** Class used to find a rectangle in m containing point and maximizing score
*
*	@param m The matrix in which to search the rectangle
*	@param point The point that should be contained within the first rectangle
*	@param score A score function that should give higher scores to good rectangles
*	@param delimiter The value that should not be contained in a rectangle
*/
class MaxRectangleBuilder(m: MyMatrixLike, point: Coord, score:(Rectangle) => Double, delimiter: Int) {
	private val THRESHOLD =  3 // Max number of DELIMITER to pass over before stopping the rectangle

	private var count_row = 0 // number of delimiters we have met on a row
	private var count_col = 0 // number of delimiters we have met on a col
	private var count_adj_row = 0 // number of adjacent ones we have met on a row
	private var count_adj_col = 0 // number of adjacent ones we have met on a col

	/**
	* 	@return true if we can still move on this row
	*/
	private def canMoveRow(point: Coord, d: Direction, limit: Int): Boolean = {
		assert(d == Direction.UP || d == Direction.DOWN, s"Can only move Up or Down")
		val newPoint = point.move(d)
		// if the point is in the previous boundary
		val inLimits = if(d == Direction.UP) limit <= newPoint.row else newPoint.row <= limit

		if(m.isDefinedAtRow(newPoint.row) && m(newPoint.row, newPoint.col) == delimiter) count_row += 1
		else if(m.isDefinedAtRow(newPoint.row)) count_adj_row += 1

		// if we met 2 * THRESHOLD adjacent one, we reset the count of delimiters
		if(count_adj_row == 2 * THRESHOLD){
			count_row = 0
			count_adj_row = 0
		}

		m.isDefinedAtRow(newPoint.row) && (m(newPoint.row, newPoint.col) != delimiter || count_row <= THRESHOLD) && inLimits
	}

	/**
	* 	@return true if we can still move on this column
	*/
	private def canMoveCol(point: Coord, d: Direction): Boolean = {
		assert(d == Direction.LEFT || d == Direction.RIGHT, s"Can only move left or right")
		val newPoint = point.move(d)

		if(m.isDefinedAtCol(newPoint.col) && m(newPoint.row, newPoint.col) == delimiter) count_col += 1
		else if(m.isDefinedAtCol(newPoint.col)) count_adj_col += 1

		if(count_adj_col == 2 * THRESHOLD){
			count_col = 0
			count_adj_col = 0
		}

		m.isDefinedAtCol(newPoint.col) && (m(newPoint.row, newPoint.col) != delimiter || count_col <= THRESHOLD)
	}

	/**
	* 	@param point Point for which we are finding the boundaries
	* 	@param oldBoundaries Previous boundary
	* 	@return The boundaries for this point
	*/
	private def findBoundaries(point: Coord, oldBoundaries: Boundary):Boundary = {
			// reset the count of delimiters on the rows
			count_row = 0

			// points that will respectvely move up and down
			var up = Coord(point.row, point.col)
			var down = Coord(point.row, point.col)

			var canMoveUp = canMoveRow(up, Direction.UP, oldBoundaries.up)
			var canMoveDown = canMoveRow(down, Direction.DOWN, oldBoundaries.down)

			while(canMoveUp || canMoveDown){
				if(canMoveUp){
					up = Coord(up.row - 1, up.col)
					canMoveUp = canMoveRow(up, Direction.UP, oldBoundaries.up)
				}
				if(canMoveDown){
					down = Coord(down.row + 1, down.col)
					canMoveDown = canMoveRow(down, Direction.DOWN, oldBoundaries.down)
				}
			}

			Boundary(up.row, down.row)
	}

	/**
	 * @param a upper or lower row of the Rectangle
	 * @param left left Boundary of the rectangle
	 * @param right right boundary of the rectangle
	 * @param lim if a is the upper limit, then lim is the lower limit of the rectangle
	 *				if a is the lower limit, then lim is the upper limit of the rectangle
	 * @param 1 if we should go down and -1 otherwise
	 * @return the new upper or lower row of the rectangle
	 */
	private def shrink(a: Int, left: Int, right: Int, lim: Int, down: Int):Int = {
		var res = a
		if(res == lim) return res // Submatrix of a single line
		var rec = Rectangle(Coord(a, left), Coord(a, right))
		var density = rec.density(m)

		// removes rows while their density is lower to 0.75 or while the limit is not reached
		while(density < 0.75 && (res + down) != lim){
			res = res + down
			rec = Rectangle(Coord(res, left), Coord(res, right))
			density = rec.density(m)
		}

		res
	}

	/** Removes the upper and lower rows of rect that do not have a density of ones
	* 	of at least 0.85. Stops when the upper and lower rows have a density of 0.85 or when there is
	* 	just one row left
	*	
	*	@param rect The rectangle that needs post processing
	*/
	private def postprocessing(rect: Rectangle):Rectangle = {
		var new_up = rect.topLeft.row
		var new_down = rect.bottomRight.row
		var new_left = rect.topLeft.col
		var new_right = rect.bottomRight.col

		new_up = shrink(new_up, new_left, new_right, new_down, 1)
		new_down = shrink(new_down, new_left, new_right, new_up, -1)

		Rectangle(Coord(new_up, new_left), Coord(new_down, new_right))
	}

	/**
	*	@return a rectangle whose top left point has coordinate (boundaries.up, pointLeft.col)
	*	and bottom right point has coordinate (boundaries.down, pointRight.col)
	*/
	private def createRectangle(pointLeft: Coord, pointRight: Coord, boundaries: Boundary) = {
		Rectangle(Coord(boundaries.up, pointLeft.col), Coord(boundaries.down, pointRight.col))
	}

	/**
	* 	@param d direction in which to find the boundaries
	* 	@return all the boundaries in direction d
	*/
	private def findBoundariesDirection(d: Direction):ArrayBuffer[Boundary] = {
		count_col = 0
		var boundaryP = findBoundaries(point,Boundary(0,m.rows-1))
		boundaryP.setCol(point.col) // column associated with this boundary
		var boundary = boundaryP
		var boundaries = ArrayBuffer[Boundary]()
		var pointNew = point

		while(canMoveCol(pointNew, d)){
			pointNew = pointNew.move(d)
			boundaryP = boundary
			boundary = findBoundaries(pointNew, boundary)
			boundary.setCol(pointNew.col) // column associated with this boundary

			if(boundary != boundaryP){
				boundaries = boundaries :+ boundaryP
			}
		}
		boundaries :+ boundary
	}

	/**
	* 	@return a rectangle going from the left to the right boundaries
	*/
	private def findRectangle(right: Boundary, left: Boundary) = {
		val up = Math.max(right.up, left.up)
		val down = Math.min(right.down, left.down)
		createRectangle(Coord(up, left.col),Coord(down, right.col), Boundary(up, down))
	}

	/** Finds the rectangle maximinzing the score
	*/
	lazy val findMaxRectangle:Rectangle = {
		// boundaries to the right and left of the point
		val right = findBoundariesDirection(Direction.RIGHT)
		val left = findBoundariesDirection(Direction.LEFT)
		// the initial max rectangle is just the point
		var maxRect = Rectangle(Coord(point.row,point.col), Coord(point.row,point.col))
		var leftIndice = 0

		for(i <- 0 until right.size){
			var stop = false
			var hasNextRight = (i + 1 < right.size)
			for(j <- leftIndice until left.size; if(!stop)){
				// use to skip rectangles that we do not have to consider
				// because their score cannot be the highest
				if(hasNextRight && right(i+1).isIncludedIn(left(j))) leftIndice = j
				if(hasNextRight && left(j).isIncludedIn(right(i+1))) stop = true
				else{
					var rec = findRectangle(right(i), left(j))
					if(score(rec) >= score(maxRect)) maxRect = rec
				}
			}
		}

		// if we accepted zeros in the rectangle we have to postprocess it
		if(THRESHOLD > 0) postprocessing(maxRect)
		else maxRect
	}
}
