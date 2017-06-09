package thesis.orderings
import thesis.matrixTypes._
import thesis.utils._

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.PriorityQueue
import breeze.linalg._
import breeze.numerics._

/**
*   Ordering that finds k rows in the matrix with maximal error such that
*   there are separated by half the size of the kernel.
*   Then use those k rows to delimit k+1 blocks.
*   Reorder the rows with a tsp
*   @param k the number of rows with maximal error to find
*   @param equalBlocks if false apply the procedure described above to find the blocks
*                       Otherwise, use k blocks of equal size
*/
class KMaxOrdering(k: Int = 10, equalBlocks: Boolean = false) extends MatrixOrdering with UsesKernelOptimization{
    override def toString:String = "KMax"

    /**
    *   @param mat The matrix on which the errors are computed
    *   @return An array containing the error for every row
    */
    private def getErrorRows(mat: MatrixMoves): Array[Double] = {
        Array.tabulate(mat.rows){ r =>
            mat.getFullConvolutionErrorSingleRow(r)
        }
    }

    /**
    *   @param mat The matrix on which the maximal error rows are computed
    *   @param filteredRows The rows that were filtered such that they are far enough away from the max row
    *   @param errors The errors corresponding to the filtered rows
    *   @return The row in filteredRows with maximal error
    */
    private def getMax(mat: MatrixMoves, filteredRows: Array[Int], errors: Array[Double]) = {
        var max = errors(filteredRows(0))
        var best_row = filteredRows(0)
        var i = 0
        while(i < filteredRows.size){
            if(errors(filteredRows(i)) > max){
                max = errors(filteredRows(i))
                best_row = filteredRows(i)
            }
            i += 1
        }

        best_row
    }

    /**
    *   @param mat the matrix on which to compute the hamming distance
    *   @param i First row index
    *   @param j Second row index
    *   @return the hamming distance between i and j in mat
    */
    private def hamming(i: Int, j: Int, mat: MatrixMoves) = {
        var count = 0
        var c = 0
        while(c < mat.cols){
            if(mat(i,c) != mat(j,c)) count += 1
            c += 1
        }

        count
    }

    /**
    *   @param rows: rows with maximal error
    *   @param mat: matrix being reordered
    *   @return an array containing the different blocks in the format
    *   upper row block i, lower row block i
    */
    private def getBlocks(rows: PriorityQueue[Int], mat: MatrixMoves): Array[Array[Int]] = {
        val size = rows.size - 1
        var next = rows.dequeue

        val blocks = Array.tabulate[Int](size,2){ case(i,j) =>
            if(j==0) next
            else if(i == size-1) {
                rows.dequeue
            }
            else {
                val temp = rows.dequeue
                // we cut the block between the rows with the greatest hamming distance
                // to keep similar rows together
                if(hamming(temp,temp+1, mat) > hamming(temp,temp-1, mat)) next = temp + 1
                else next = temp
                next - 1
            }
        }
        assert(blocks.forall{array =>
            array.forall{i =>
                mat.isDefinedAtRow(i)
            }
        })
        blocks
    }

    /**
    *   @param up Last line of the upper block
    *   @param down First line of the lower block
    *   @param i Index that must be converted
    *   @return if i is in the upper block, returns i
    *          otherwise return the line in the lower block
    */
    private def convertUpToDown(up: Int, down: Int)(i: Int): Int = {
        if(i <= up) i
        else down + (i - up) - 1
    }

    /**
    *   @param up last line of the upper block
    *   @param down first line of the lower block
    *   @param i index that must be converted
    *   @return if i is in the lower block, returns i
    *          otherwise return the line in the upper block
    */
    private def convertDownToUp(up: Int, down: Int)(i: Int): Int = {
        if(down <= i) i
        else up + (i - down) + 1
    }

    /**
    *   @param up upper block
    *   @param down lower block
    *   @param blocks array defining the bounds of the blocks
    *   @param mat matrix we are reordering
    *   @return the error obtained if block i is put above block j
    */
    private def distanceBlocks(up: Array[Int], down: Array[Int], mat: MatrixMoves) = {
        var sum = 0.0
        var n = mat.getConvolutionSettings.kernel.rows
        sum += mat.partialErrorBlocks(Math.max(up(1) - n/2 + 1, 0), up(1), convertUpToDown(up(1), down(0)))
        sum + mat.partialErrorBlocks(down(0), Math.min(down(0) + n/2 - 1, mat.rows-1), convertDownToUp(up(1), down(0)))
    }

    /**
    *   @return a matrix containing the distances between the blocks
    *   @note BEWARE! it is not always symmetric
    */
    private def getAsymmetricDistances(mat: MatrixMoves, blocks: Array[Array[Int]]): DenseMatrix[Double] = {
        DenseMatrix.tabulate[Double](blocks.size,blocks.size){ case(i,j) =>
            distanceBlocks(blocks(i),blocks(j), mat)
        }
    }

    /**
    *   @return Convert the asymmetric matrix dists used for the tsp
    *           into a symmetric distance matrix by introducing ghost rows
    */
    private def makeDistsSym(dists: DenseMatrix[Double]): DenseMatrix[Double] = {
        DenseMatrix.tabulate[Double](2*dists.rows,2*dists.rows){ case(i,j) =>
            if((i < dists.rows && j < dists.rows) || (dists.rows <= i && dists.rows <= j)) Double.MaxValue
            else if(i == (j - dists.rows) || j == (i - dists.rows)) Double.MinValue
            else if(j > i) dists(j - dists.rows,i)
            else dists(i - dists.rows,j)
        }
    }

    /**
    *   @return Remove the ghost rows from order.
    */
    private def filterDouble(order: Array[Int], mat: MatrixMoves): Array[Int] = {
        Array.tabulate(order.size/2){ i =>
            assert(Math.abs(order(2*i) - order(2*i+1)) == mat.rows)
            Math.min(order(2*i), order(2*i+1))
        }
    }

    /**
    *   @param orderBlocks order in which the blocks should be reordered
    *   @return the corresponding order of the rows
    */
    private def getPermutation(orderBlocks: Array[Int], blocks: Array[Array[Int]]): Array[Int] = {
        orderBlocks.flatMap{ b =>
            (blocks(b)(0) to blocks(b)(1))
        }
    }

    /**
    *   @inheritdoc
    */
    override def changeOrdering(mat:MatrixMoves) = {

        var previousError = Double.MaxValue

        while(Math.abs(previousError - mat.getError) > 0.01){
            previousError = mat.getError
            println(mat.getError)

            val blocks = if(!equalBlocks){
                val errors = getErrorRows(mat)


                // get k rows with highest error separated by a distance at least n
                // where n is the number or rows in the kernel
                var n = mat.getConvolutionSettings.kernel.rows
                var b = PriorityQueue[Int]()
                var filteredRows = (0 until mat.rows).toArray

                while(b.size < k && filteredRows.size > 0){
                    var maxRow = getMax(mat, filteredRows, errors)
                    // filter rows that are too close to maxRow
                    filteredRows = filteredRows.filter{ x => Math.abs(x - maxRow) > n}
                    b += maxRow
                }

                // add the first and last rows if there are not there yet
                if(b.min != 0) b += 0
                if(b.max != mat.rows-1) b += mat.rows-1

                b = b.reverse // we want the first row first

                getBlocks(b, mat)
            }
            else{
                val blockSize = Math.ceil(mat.rows.toDouble/k).toInt
                Array.tabulate[Array[Int]](k){ x =>
                    Array(x*blockSize, Math.min((x+1)*blockSize - 1, mat.rows-1))
                }
            }

            // get the distance matrix and make it symmetric
            var aSymDists = getAsymmetricDistances(mat, blocks)
            val symDists = makeDistsSym(aSymDists)

            val orderBlocks = filterDouble(TSPSolver.solve_tsp(symDists), mat)
            mat.permute(getPermutation(orderBlocks, blocks))
            mat.t
        }

        if(mat.isTranspose) mat.t
        else mat
    }
}
