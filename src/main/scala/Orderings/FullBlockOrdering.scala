package thesis.orderings
import thesis.matrixTypes._
import thesis.utils._

import scala.collection.mutable.ArrayBuffer

/** Performs a Block ordering on the rows and columns of the whole matrix and then
 *  tries to improve the solution with optimizeBlocks
 */
class FullBlockOrdering extends MatrixOrdering{

    override def toString:String = "Block"

    /**
     * @param a matrix that was reordered
     * @param nBlocks number of blocks used for the reordering
     * @return an optimized matrix. For each block, tries to reverse the order of its rows.
     * Indeed the blocks ordering aggregates the rows of each blocks into a single row but does not consider
     * the order of the rows in a block. Sometimes it should be reversed to yield a better solution
     */
    private def optimizeBlocks(a: MatrixMoves, nBlocks: Int) = {
        var computeSingleError = Utils.getSingleErrorFunction(a.getConvolutionSettings.errorType)

        val sizeBlocksRow = a.rows / nBlocks // number of rows per blocks

        for(i <- 0 until nBlocks){
            var count1 = 0.0
            // compute error of this block
            for(c <- 0 until a.cols){
                count1 += computeSingleError(a(i * sizeBlocksRow, c), a.convoluteSinglePoint(i * sizeBlocksRow, c))
            }

            // tries to reverse it
            val rev = if(i < nBlocks -1) a.reverse(i * sizeBlocksRow,sizeBlocksRow - 1 + i * sizeBlocksRow)
                      else a.reverse((nBlocks - 1) * sizeBlocksRow,a.rows - 1)

            // recomputes the error for this block after being reversed
            var count2 = 0.0
            for(c <- 0 until a.cols){
                count2 += computeSingleError(a(i * sizeBlocksRow, c), a.convoluteSinglePoint(i * sizeBlocksRow, c))
            }

            // keep the best order
            if(count2 >= count1 ) rev.reverseTheMove
        }
    }

    /**
    * @inheritdoc
    */
    override def changeOrdering(mat:MatrixMoves) = {
        if(mat.isCat) throw new IllegalArgumentException("Block ordering only works for binary data")
         val nBlocks = 10

         // change the settings to make them compatible with the brute force ordering
         val backUpSettings = mat.getConvolutionSettings
         mat.setConvolutionSettings(ConvolutionSettings(Kernels.gaussianBlurSmooth(3,3), ErrorType.Abs, Padding.Boundary))
        lazy val bruteForce = new BruteForceImproved()

        // use a block ordering with a brute force ordering
        val block = new BlockOrdering(nBlocks, squash=(a:Array[Double]) => {
        	// Squashes a the array into a single number
      		val nOne = a.count(_ == 1.0)
      		val nZeros = a.length - nOne
      		if(nZeros > nOne) 0.0
      		else 1.0
        }, bruteForce)

        // reorder the rows with a Block Ordering
        block.changeOrdering(mat)
        optimizeBlocks(mat, nBlocks)

        // reorder the columns with a Block Ordering
        block.changeOrdering(mat.t)
        optimizeBlocks(mat, nBlocks)

        // recover the settings
        mat.setConvolutionSettings(backUpSettings)
        mat.getFullConvolutionError
        mat
    }

}
