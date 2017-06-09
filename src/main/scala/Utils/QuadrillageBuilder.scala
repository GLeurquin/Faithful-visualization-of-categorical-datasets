package thesis.utils

import thesis.rectangles._
import thesis.matrixTypes._

object QuadrillageBuilder{
	/**
	*	Scoring function 1
	*/
	val densityScore = (r:Rectangle,density:Double, m:MyMatrixLike) => {
		val densityDiff = Math.abs(density - 0.5)
		densityDiff
	}

	val score2 = (r:Rectangle,density:Double, m:MyMatrixLike) => {
		lazy val normalizedArea = r.area/(m.rows * m.cols).toDouble
		density * Math.sqrt(normalizedArea)
	}

	val score1 = (r:Rectangle,density:Double, m:MyMatrixLike) => {
		val densityDiff = Math.abs(density - 0.5)
		lazy val normalizedArea = r.area/(m.rows * m.cols).toDouble
		densityDiff * Math.sqrt(normalizedArea)
	}
}

/** Class that can split a matrix in 4 (or possibly less) rectangles according to their density
*	and a scoring function
*	
*	@param score: a scoring function used to choose the ideal rectangle that returns its score (>=0) given a rectangle and its density
*/
class QuadrillageBuilder(m:MyMatrixLike, score:(Rectangle, Double, MyMatrixLike) => Double){

	private val DEBUG = true

	/**
	*	@param rows_rect The number of rows a rectangle of the quadrillage should contain.
	*	The last rectangle will be truncated if rows_rect does not divide m.rows
	*	@param cols_rect The number of columns a rectangle of the quadrillage should contain.
	*	The last rectangle will be truncated if cols_rect does not divide m.cols
	*	@return A 2D array of rectangles describing the quadrillage
	*/
	private def quadrill(rows_rect:Int, cols_rect:Int):Array[Array[Rectangle]] = {
		val row_ranges = Utils.makeBatches((0 to m.rows), rows_rect).sliding(2).toArray
		val col_ranges = Utils.makeBatches((0 to m.cols), cols_rect).sliding(2).toArray

		Array.tabulate[Rectangle](row_ranges.length, col_ranges.length){case(i, j) =>
			val Vector(rmin, rmax) = row_ranges(i)
			val Vector(cmin, cmax) = col_ranges(j)

			val topLeft = Coord(rmin, cmin)
			val bottomRight = Coord(rmax-1, cmax-1)
			Rectangle(topLeft, bottomRight)
		}
	}

	private case class DensityInfo(rectangle:Rectangle, numBlack:Double) {
		lazy val density = numBlack.toDouble/rectangle.area
		lazy val scoreValue = score(rectangle, density, m)

		def +(other:DensityInfo):DensityInfo = {
			val newRectangle = rectangle + other.rectangle
			val newNumBlack = numBlack + other.numBlack

			DensityInfo(newRectangle, newNumBlack)
		}

		def -(other:DensityInfo):DensityInfo = {
			val newRectangle = rectangle - other.rectangle
			val newNumBlack = numBlack - other.numBlack

			DensityInfo(newRectangle, newNumBlack)
		}
	}

	/** Splits the matrix in 4 rectangles of maximising the scoring function
	*	May return 1 rectangles only if the first rectangle found spans the full rows and columns
	*	May return 2 rectangles only if the first rectangle found spans the full columns
	*	May return 3 rectangles only if the first or second rectangle found spans the full rows
	*
	*	@param rows_rect: @see quadrill
	*	@param cols_rect: @see quadrill
	*/
	def makeQuadrillage(rows_rect:Int, cols_rect:Int):List[Rectangle] = {
		val quadrillage = quadrill(rows_rect, cols_rect)

		// if(DEBUG){
		// 	println("Quadrillage")
		// 	quadrillage.foreach{row=>println(row.mkString(","))}
		// }

		// Create the density information
		val densityInfos = quadrillage.map{row =>
			row.map{rectangle=>
				DensityInfo(rectangle, rectangle.numBlack(m))
			}
		}

		val nGridRow = densityInfos.length
		val nGridCol = densityInfos(0).length

		val fullDensities = Array.ofDim[DensityInfo](densityInfos.length, densityInfos(0).length)

		var bestCoord = (0,0) // Coordinates of the rectangle in fullDensities
		var bestScore = -1.0

		/*
		*	Find the first rectangle from the top left, going right
		*/
		(0 until nGridRow).foreach{ i =>
			(0 until nGridCol).foreach{ j =>
				val densityValue = (i, j) match {
					case (0,0) => densityInfos(0)(0)
					case (_,0) => fullDensities(i-1)(0)+densityInfos(i)(0)
					case (0,_) => fullDensities(0)(j-1)+densityInfos(0)(j)
					case _ => {
						val left = fullDensities(i)(j-1)
						val above = fullDensities(i-1)(j)
						val diag = fullDensities(i-1)(j-1)
						val here = densityInfos(i)(j)

						left + (above-diag + here)
					}
				}

				fullDensities(i)(j) = densityValue

				if(densityValue.scoreValue > bestScore){
					bestScore = densityValue.scoreValue
					bestCoord = (i, j)
				}
			}
		}

		val topLeftRectangle = fullDensities(bestCoord._1)(bestCoord._2).rectangle
		val minCol = bestCoord._2+1
		var listOfRectangles = List[Rectangle](topLeftRectangle)

		if(DEBUG){
			println("\nFor top left rectangle")
			// For debug only
			println("Densities")
			fullDensities.map(_.map(x => f"${x.density}%2.2f")).foreach{row => println(row.mkString(" "))}

			println("\nScores")
			fullDensities.map(_.map(x => f"${x.scoreValue}%2.2f")).foreach{row => println(row.mkString(" "))}
		}


		lazy val bottomLeftRectangle_topLeft = Coord(topLeftRectangle.bottomRight.row+1, 0)
		lazy val bottomLeftRectangle_bottomRight = Coord(m.rows-1, topLeftRectangle.bottomRight.col)
		lazy val bottomLeftRectangle = Rectangle(bottomLeftRectangle_topLeft, bottomLeftRectangle_bottomRight)

		if(topLeftRectangle.bottomRight.row != m.rows-1) listOfRectangles = listOfRectangles :+ bottomLeftRectangle
		if(topLeftRectangle.bottomRight.col == m.cols-1) return listOfRectangles // Only 1 or 2 rectangles

		// RESET
		bestCoord = (0,0) // Coordinates of the rectangle in fullDensities
		bestScore = -1.0

		/*
		*	Find the second rectangle from the top right, going left
		*/
		(0 until nGridRow).foreach{ i =>
			 // Only search on the remaining columns
			(minCol until nGridCol).reverse.foreach{ j =>
				val densityValue = (i, j) match {
					case (0,j) if j==nGridCol-1 => densityInfos(0)(j)
					case (_,j) if j==nGridCol-1 => fullDensities(i-1)(j)+densityInfos(i)(j)
					case (0,_) => fullDensities(0)(j+1)+densityInfos(0)(j)
					case _ => {
						val right = fullDensities(i)(j+1)
						val above = fullDensities(i-1)(j)
						val diag = fullDensities(i-1)(j+1)
						val here = densityInfos(i)(j)

						right + (above-diag + here)
					}
				}

				fullDensities(i)(j) = densityValue

				// Only consider rectangles whose border is next to the first rectangle
				if(j == minCol && densityValue.scoreValue > bestScore){
					bestScore = densityValue.scoreValue
					bestCoord = (i, j)
				}
			}
		}

		val topRightRectangle = fullDensities(bestCoord._1)(bestCoord._2).rectangle
		listOfRectangles = listOfRectangles :+ topRightRectangle

		lazy val bottomRightRectangle_topLeft = Coord(topRightRectangle.bottomRight.row+1, topRightRectangle.topLeft.col)
		lazy val bottomRightRectangle_bottomRight = Coord(m.rows-1, topRightRectangle.bottomRight.col)
		lazy val bottomRightRectangle = Rectangle(bottomRightRectangle_topLeft, bottomRightRectangle_bottomRight)

		if(topRightRectangle.bottomRight.row != m.rows-1) listOfRectangles = listOfRectangles :+ bottomRightRectangle

		listOfRectangles
	}


}
