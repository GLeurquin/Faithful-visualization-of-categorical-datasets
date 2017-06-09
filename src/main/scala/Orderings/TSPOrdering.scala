package thesis.orderings
import thesis.matrixTypes._
import thesis.utils._

import breeze.linalg._
import breeze.numerics._
import scala.collection.mutable.ArrayBuffer

class TSPOrdering(distanceR: (DenseMatrix[Double], Int, Int) => Double, distanceC: (DenseMatrix[Double], Int, Int) => Double, row: Boolean = true, col: Boolean = true) extends MatrixOrdering{

    override def toString:String = "TSP"

    /**
    *   @param m The matrix we want to reorder
    *   @param f A distance function
    *   @return A distance matrix for the rows
    */
    private def getAdjacency(m:DenseMatrix[Double], f: (DenseMatrix[Double], Int, Int) => Double) = {
        val res = DenseMatrix.zeros[Double](m.rows, m.rows)
        for(i <- 0 until m.rows; j <- i+1 until m.rows){
            val temp = f(m,i,j)
            res(i,j) = temp
            res(j,i) = temp
        }
        res
    }

    /**
    * @inheritdoc
    */
    override def changeOrdering(mat:MatrixMoves) = {
        if(row){
            // get the distance matrix for the rows
            val adjRow = getAdjacency(mat.getMatrix, distanceR)
            val orderRows = TSPSolver.solve_tsp(adjRow)
            mat.permute(orderRows)
        }
        if(col) {
            // get the distance matrix for the columns
            val adjCol = getAdjacency(mat.t.getMatrix, distanceC)
              mat.permute(TSPSolver.solve_tsp(adjCol))
              mat.t
        }

        var a = mat

        if(mat.isCat && !mat.dontChangeColors){
            // tries to change the colors of the to reduce the number of transitions
            mat.changeColors
        }
        else if(!mat.isCat){
            // tries to reverse the rows to check if the error can be decreased
            // needed because TSP does not take the padding into account
            var error = mat.getError
            val rev1 = mat.reverse(0,mat.rows-1)
            if(error < mat.getError) rev1.reverseTheMove

            error = mat.getError

            // does the same for the columns
            mat.t
            val rev2 = mat.reverse(0, mat.rows-1)
            if(error < mat.getError) rev2.reverseTheMove
            mat.t
        }
        mat.setError(mat.getFullConvolutionError)
        mat
    }
}
