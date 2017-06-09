package thesis.orderings

import thesis.rectangles._
import thesis.matrixTypes._
import thesis.utils._

import scala.collection.GenSeq

/**
* 	@param m the submatrix
*	@param r the rectangle with respect to the submatrix : (0,0) is the top left of THIS submatrix (not the original)
*	@param goUp The list of sub matrix decisions allowing to go back to the original matrix
*/
case class QuadrillageHelper(m:MatrixMoves, r:Rectangle) extends MatrixHelpers{

	var rowOffset = 0
	var colOffset = 0

	/**
	* 	@return the rectangle with respect to the original matrix
	*/
	def originalRectangle = {
		val newTopLeft = Coord(r.topLeft.row+rowOffset, r.topLeft.col+colOffset)
		val newBottomRight = Coord(r.bottomRight.row+rowOffset, r.bottomRight.col+colOffset)
		Rectangle(newTopLeft, newBottomRight)
	}

	def visualizeMatrix = visualize(m, s"${m.desc}-${originalRectangle}")
	def visualizeMatrixRescaled = visualizeRescaled(m, s"${m.desc}-${originalRectangle}")
}

/**	Allows to do a quarillage ordering by splitting the matrix into 2^iterations submatrices and applying otherOrdering to each of them
*	
*	@param The output will be maximum 4^iterations different matrices. Should be max 2
*	@param otherOrdering The ordering to apply on the submatrices
*	@param score The score to use to select how the submatrices are chosen
*	@param rowMeshDivider The divider of the rows, determining the mesh on which the ordering will be applied
*	@param colMeshDivider The divider of the cols, determining the mesh on which the ordering will be applied 
*/
class QuadrillageOrdering(iterations: Int, otherOrdering:MatrixOrdering, score:(Rectangle,Double, MyMatrixLike)=>Double, rowMeshDivider:Int=10, colMeshDivider:Int=10){
	private val DEBUG = true

	require(iterations < 3, "Are you sure you want more than (possibly) 64 matrices ?")

	/** splits the matrix in 4, <Iterations> times.
	*	
	*	@param shouldSplit A function that returns true if
	*			the algorithm should further split the submatrix into 4 (or less) submatrices
	*	@return the submatrices, their corresponding rectangle and
	*			the list of decisions to apply to reinsert it into the original matrix (in reverse order)
	*/
	private def getMesh(m:MatrixMoves, shouldSplit:(MatrixMoves, Rectangle) => Boolean = {(m,r)=>true}):GenSeq[QuadrillageHelper] = {
		val initRectangle = Rectangle(Coord(0,0), Coord(m.rows-1,m.cols-1))
		var listOfMatrices = List[QuadrillageHelper](QuadrillageHelper(m, initRectangle))

		(0 until iterations).foreach{i =>
			listOfMatrices = listOfMatrices.flatMap{quadrillageHelper =>
				val subM = quadrillageHelper.m

				val previousRect = quadrillageHelper.r

				if(!shouldSplit(subM, previousRect)){
					List(quadrillageHelper)
				}
				else{
					val qb = new QuadrillageBuilder(subM,score)

					/**
					* 	Makes a mesh that splits the number of rows and columns by 10.
					*	The mesh is then aggregated to give the 1,2,3 or 4 sub-rectangles
					*	These rectangles are with respect to subM : (0, 0) will be the top left of subM, not of the original matrix!
					*/

					val rowMesh = Math.max(subM.rows/rowMeshDivider,1)
					val colMesh = Math.max(subM.cols/colMeshDivider,1)

					if(DEBUG) {
						println()
						println(s"Mesh of rectangles of size $rowMesh x $colMesh")
						println(s"Splitting ${quadrillageHelper.originalRectangle} into")
					}

					val quadrillage = qb.makeQuadrillage(rowMesh,colMesh)


					/**
					*	Get the submatrices corresponding to the quadrillage
					*/
					quadrillage.map{subRect =>
						var newSubM:MatrixMoves = subM.copy

						// subRect's coordinates are with respect to subM !
						newSubM.subMatrix(subRect)

						// Reverse the newSubMatrixDecision, because its first element should be reversed before the second
						val qh = QuadrillageHelper(newSubM, subRect)
						qh.rowOffset = quadrillageHelper.rowOffset + previousRect.topLeft.row
						qh.colOffset = quadrillageHelper.colOffset + previousRect.topLeft.col
						if(DEBUG) println(qh.originalRectangle)

						qh
					}
				}
			}
		}

		listOfMatrices
	}

	/**
	*	@param m The matrix on which to apply the mesh
	*	@see getMesh
	*	@return a list of QuadrillageHelper representing the submatrices and their relative positions with regard to m
	*/
	def applyMesh(m:MatrixMoves,shouldSplit:(MatrixMoves, Rectangle) => Boolean = {(m,r)=>true}) = {
		val submatrices = getMesh(m, shouldSplit) // List of submatrices

		println(s"\nWill compute ${submatrices.length} times the ordering for those rectangles:")
		submatrices.foreach{x=>println(x.originalRectangle)}
		println()

		// Run the ordering only on the last rectangles
		submatrices.par.map{qh =>
			val newM = otherOrdering.changeOrdering(qh.m)
			 // reverse the decisions to reinsert the submatrices in the correct order
			newM.insertAllSubmatrix()

			QuadrillageHelper(newM, qh.originalRectangle)
		}.toList
	}

}
