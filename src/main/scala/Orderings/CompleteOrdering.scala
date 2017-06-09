package thesis.orderings
import thesis.matrixTypes._
import thesis.utils._
import thesis.rectangles._

import scala.collection.mutable.ArrayBuffer

/** Parallelized version of the local search
*   @param moveUpdate should the weights of the moves be dynamically updated ?
*/
class CompleteOrdering(moveUpdate:Boolean = true) extends MatrixOrdering with UsesKernelOptimization {

    private val movesList = Array(Move.SwapRectangles, Move.Swap, Move.SwapAdjacent,Move.Reverse, Move.Relocate).map(x => MoveProba(x))
    private val ls = new LocalSearch(5000,movesList,onlyRows = true, autostopGradient = -0.0001, moveUpdate=moveUpdate)

    override def toString:String = s"Complete-$ls"

    /**
    *   @param m matrix to Reorder
    *   @param nBlocks number of threads to use to reorder the rows/columns
    *   @return a reordered matrix
    */
    private def findOrdering(m: MatrixMoves, nBlocks: Int) = {
        val sizeBlockRows = m.rows / nBlocks
        val sizeBlockCols = m.cols / nBlocks

        // cut the rows and the columns of the matrix into nBlocks
        // create copies for each submatrix
        val listSubs = List.tabulate(2,nBlocks){ case (h, i) =>
            val sizeBlock = if(h==0) sizeBlockRows else sizeBlockCols
            val mCopy = m.copy
            if(h==1) mCopy.t
            val topLeft = Coord(i*sizeBlock, 0)
            val bottomRight = Coord((i+1)*sizeBlock-1, mCopy.cols-1)
            val r = Rectangle(topLeft, bottomRight)
            mCopy.subMatrix(r)
            mCopy
        }

        // keep a copy of the orderings of the original matrix
        val rowsCopy = m.getRows.clone
        val colsCopy = m.getCols.clone

        // reorder the rows and then the columns
        listSubs.foreach{ subList =>
            // reorder the rows in parallel with nBlocks threads
            subList.par.foreach(ls.changeOrdering(_))
        }

        // apply the changes to the original matrix
        listSubs.zipWithIndex.par.foreach{ case(subList, h) =>
            subList.zipWithIndex.foreach{ case(sub, idx) =>
                for(r <- 0 until sub.getRows.size){
                    if(h==0) rowsCopy(r+idx*sizeBlockRows) = sub.getRows(r)
                    else colsCopy(r+idx*sizeBlockCols) = sub.getRows(r)
                }
            }
        }

        m.permute(rowsCopy, false)
        m.t
        m.permute(colsCopy, false)
        m.t
        m
    }

    /**
     * @param mat matrix that should be compared with best
     * @param best best matrix found so far
     * @return the matrix with the lowest error between mat and best
     */
    private def updateBest(mat: MatrixMoves, best: MatrixMoves): MatrixMoves = {
        if(mat.getError >= best.getError) best
        else mat.copy
    }

    /**
    *   @inheritdoc
    */
    override def changeOrdering(m:MatrixMoves) = {
        var best: MatrixMoves = m.copy
        val full = new KMaxOrdering(10, true)
        if(!m.isCat) full.changeOrdering(m)
        best = updateBest(m, best)
        var mat = findOrdering(m,3)
        best = updateBest(m, best)
        mat = findOrdering(mat,2)
        best = updateBest(m, best)
        if(!m.isCat) full.changeOrdering(mat)
        best = updateBest(m, best)
        mat = findOrdering(mat,3)
        best = updateBest(m, best)
        mat = findOrdering(mat,1)
        best = updateBest(m, best)
        best
    }

}
