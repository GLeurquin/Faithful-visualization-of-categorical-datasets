package thesis.rectangles

import thesis.matrixTypes._
import thesis.utils._


/** Class representing a matrix coordinate
*/
case class Coord(row:Int, col:Int){
	/** Moves the current coordinate one row or col in direction d
	*/
	def move(d:Direction):Coord = {
		Coord(row+d.row, col+d.col)
	}
	override def toString = s"[$row, $col]"
}

/** Class that represents a rectangle
*	Matrix has (0,0) in top left
*/
case class Rectangle(topLeft:Coord, bottomRight:Coord) extends MatrixHelpers{
	require(topLeft.col <= bottomRight.col, s"$this has a negative width")
	require(topLeft.row <= bottomRight.row, s"$this has a negative height")

	private var color = -1

	/**
	*	@return The color of this rectangle
	*/
	def getColor = color

	/** Sets the color of this rectangle to c
	*	@param c The color to set
	*/
	def setColor(c: Int): Unit = {
		color = c
	}

	/**
	*	@return true if the rectangle is included in the matrix
	*/
	def isDefinedInMatrix(m:MyMatrixLike) = {
		m.isDefinedAtRow(topLeft.row) &&
		m.isDefinedAtRow(bottomRight.row) &&
		m.isDefinedAtCol(topLeft.col) &&
		m.isDefinedAtCol(bottomRight.col)
	}

	/**
	*	@return the number of black(1) pixels in this rectangle
	*/
	def numBlack(m:MyMatrixLike) = {
		var blacks = 0
		val tl = topLeft
		var br = bottomRight

		for(r <- tl.row to br.row; c <- tl.col to br.col){
			if(m(r,c) == 1) blacks += 1
		}
		blacks
	}

	/** Computes the density of black points in rect
	*
	*	@param m The matrix on which the density of this rectangle should be computed
	*/
	def density(m:MyMatrixLike) = numBlack(m).toDouble / area

	/** Merges this rectangle with the other rectangle
	*
	*	@return a new rectangle that is the merge of this rectangle and the other
	*/
	def +(other:Rectangle):Rectangle = {
		val sameColumnMerge = this.touchRows(other) && this.topLeft.col==other.topLeft.col && this.bottomRight.col==other.bottomRight.col
		lazy val sameRowMerge = this.touchCols(other) && this.topLeft.row==other.topLeft.row && this.bottomRight.row==other.bottomRight.row
		require(sameColumnMerge || sameRowMerge, s"Can't merge two rectangles if they are not next to each other and of the same width/height. this:$this, other:$other")

		if(sameColumnMerge){
			val newTopLeft = Coord(this.topLeft.row.min(other.topLeft.row), topLeft.col)
			val newBottomright = Coord(this.bottomRight.row.max(other.bottomRight.row), bottomRight.col)
			Rectangle(newTopLeft, newBottomright)
		}
		else{
			// Row merge
			val newTopLeft = Coord(topLeft.row, this.topLeft.col.min(other.topLeft.col))
			val newBottomright = Coord(bottomRight.row, this.bottomRight.col.max(other.bottomRight.col))
			Rectangle(newTopLeft, newBottomright)
		}
	}

	/** Removes the other rectangle from this rectangle
	*
	*	@return a new rectangle that is the merge of this rectangle and the other
	*/
	def -(other:Rectangle):Rectangle = {
		val sameColumnSplit = this.topLeft.col==other.topLeft.col && this.bottomRight.col==other.bottomRight.col
		lazy val sameRowSplit = this.topLeft.row==other.topLeft.row && this.bottomRight.row==other.bottomRight.row

		val otherOnLeft = topLeft.col==other.topLeft.col && bottomRight.col > other.bottomRight.col
		lazy val otherOnRight = bottomRight.col==other.bottomRight.col && topLeft.col < other.topLeft.col
		val otherOnTop = topLeft.row==other.topLeft.row && bottomRight.row > other.bottomRight.row
		lazy val otherOnBottom = bottomRight.row==other.bottomRight.row && topLeft.row < other.topLeft.row

		val okColumnSplit = sameColumnSplit && (otherOnTop || otherOnBottom)
		lazy val okRowSplit = sameRowSplit && (otherOnLeft || otherOnRight)

		require(okColumnSplit || okRowSplit, s"Can't split two rectangles if they don't share an edge, and the other is included in the former. this:$this, other:$other")

		if(okColumnSplit){
			if(otherOnTop){
				val newTopLeft = Coord(other.bottomRight.row+1, topLeft.col)
				val newBottomright = Coord(bottomRight.row, bottomRight.col)
				Rectangle(newTopLeft, newBottomright)
			}
			else{
				// Other on bottom
				val newTopLeft = Coord(topLeft.row, topLeft.col)
				val newBottomright = Coord(other.topLeft.row-1, bottomRight.col)
				Rectangle(newTopLeft, newBottomright)
			}
		}
		else{
			// Row merge
			if(otherOnLeft){
				val newTopLeft = Coord(topLeft.row, other.bottomRight.col+1)
				val newBottomright = Coord(bottomRight.row, bottomRight.col)
				Rectangle(newTopLeft, newBottomright)
			}
			else{
				// Other on right
				val newTopLeft = Coord(topLeft.row, topLeft.col)
				val newBottomright = Coord(bottomRight.row, other.topLeft.col-1)
				Rectangle(newTopLeft, newBottomright)
			}
		}
	}

	/**
	*	@param c The coordinate to test
	*	@return true if this rectangle contains coordinate c
	*/
	def contains(c:Coord):Boolean = {
		topLeft.col <= c.col && c.col <= bottomRight.col && topLeft.row <= c.row && c.row <= bottomRight.row
	}

	lazy val toRowRange:Range = topLeft.row to bottomRight.row
	lazy val toColRange:Range = topLeft.col to bottomRight.col

	/**
	*	@param other The other rectangle
	*	@return true if this rectangle is fully included in the other rectangle
	*/
	def includedIn(other: Rectangle): Boolean = {
		other.topLeft.col <= topLeft.col && bottomRight.col <= other.bottomRight.col
	}

	def intervalOverlapCols(other: Rectangle) = 1 + math.min(bottomRight.col, other.bottomRight.col) - math.max(topLeft.col, other.topLeft.col)

	/**
	*	@return true if this rectangle rows intersect the rows of the other rectangle
	*/
	def overlapsRows(other:Rectangle):Boolean = !isAbove(other) && !isBelow(other)
	
	/**
	*	@return true if this rectangle columns intersect the columns of the other rectangle
	*/
	def overlapsCols(other:Rectangle):Boolean = !isToTheLeftOf(other) && !isToTheRightOf(other)

	/**
	*	@return True if the two rectangles are row consecutive
	*/
	def touchRows(other:Rectangle):Boolean = if(isAbove(other)) 	 	bottomRight.row+1==other.topLeft.row
											 else if(isBelow(other))	other.bottomRight.row+1==topLeft.row
											 else false
	/**
	*	@return True if the two rectangles are column consecutive
	*/
	def touchCols(other:Rectangle):Boolean = if(isToTheLeftOf(other)) 	 	bottomRight.col+1==other.topLeft.col
											 else if(isToTheRightOf(other))	other.bottomRight.col+1==topLeft.col
											 else false

	/**
	*	@return True if this rectangle is to the left of the other rectangle
	*/									 
	def isToTheLeftOf(other:Rectangle):Boolean = bottomRight.col < other.topLeft.col
	
	/**
	*	@return True if this rectangle is to the left of the other rectangle
	*/	
	def isToTheRightOf(other:Rectangle): Boolean = other.bottomRight.col < topLeft.col
	
	/**
	*	@return True if this rectangle is above of the other rectangle
	*/	
	def isAbove(other:Rectangle) = bottomRight.row < other.topLeft.row
	
	/**
	*	@return True if this rectangle is below of the other rectangle
	*/	
	def isBelow(other:Rectangle) = other.bottomRight.row < topLeft.row
	
	lazy val width = bottomRight.col+1 - topLeft.col
	lazy val height = bottomRight.row+1 - topLeft.row

	lazy val area = width*height
	lazy val perimeter = (width+height)*2
	assert(area >= 1, s"$this has a negative (or null) area : $area")
}
