package thesis.utils


import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Stack
import scala.collection.mutable.HashMap

/** Performs a branch and bound on the given matrix
*/
class BranchAndBoundImproved(a: MatrixMoves) {

    private val kernel = a.getConvolutionSettings.kernel.matrix
    assert(kernel.cols <= 3, "the kernel's size should be at max 3")
    // cache for the error of each row
    private val cache = HashMap[String, Double]()

    /** Represents a node in the branch and bound tree
    *   @param partial partial solution so far
    *   @param remaining rows that still have to be chosen
    *   @param error error of the partial solution of this node
    *   @param depth depth of this node (i.e. size of the partial solution)
    */
    private case class Node(partial: Array[Int], remaining: scala.collection.mutable.Set[Int], error: Double, depth: Int){
        private var prev_val = -1 // previous row that we chose when generating children
        private val iterator = remaining.toArray.iterator // iterator used to generate the children
        val lower: Double = if(depth >= a.rows-2) 0 else lowerBound(partial, remaining.toArray,depth-1,a.rows-1)
        def getLowerBound = error + lower // get lower bound for this solution

        /**
        *   @return true if there is still at least one child to generate
        */
        def hasNext: Boolean = {
            if(!iterator.hasNext && prev_val != -1) remaining.add(prev_val) // backtrack, recover remaining
            iterator.hasNext
        }

        /**
        *   @return the next child
        */
        def getNext: Node = {
            if(prev_val != -1) remaining.add(prev_val) // backtrack, recover remaining
            prev_val = iterator.next
            remaining.remove(prev_val) // this row cannot be chosen anymore
            partial(depth) = prev_val
            var newError = updateError(error, partial, depth) // compute error of new partial solution
            Node(partial, remaining, newError, depth + 1)
        }
        /**
        *   @return The error of this node
        */
        def getError: Double = error

        /**
        *   @return true if the solution is complete
        */
        def isComplete: Boolean = depth == a.rows-1
    }

    @volatile var upper: Double = a.getError // best solution found so far

    /** Starts the branch and bound search
    */
    def run = {
        var bestValAll = a.getError // best value found in the local search
        var bestAll = Array.tabulate(a.rows){i => i} // best associated solution

        // generate all the pairs of first and last rows
        var pairs = ArrayBuffer[(Int, Int)]()
        for(i <- 0 until a.rows; j <- i + 1 until a.rows){
            pairs = pairs :+ (i,j) :+ (j,i)
        }

        /* 
        *   first and last rows are already fixed
        *   implemented this way to allow the search to be parallelized easily
        *   and to be able to remove symmetries easily
        */
        val results = pairs.toList.map{ case(first, last) =>
            branchAndBound(first,last)
        }

        val (bestVal, best) = results.foldLeft((bestValAll, bestAll)){ case(acc, a) =>
            if(a._1 < acc._1) a
            else acc
        }

       best
    }

    private def branchAndBound(first: Int, last: Int) = {
        val remaining = scala.collection.mutable.Set[Int]() // rows that have not been chosen yet
        // first and last rows already fixed so they should not be in remaining
        (0 until a.rows).foreach{ x =>
            if(x != first && x != last) remaining.add(x)
        }
        var partial = Array.fill(a.rows)(-1) // partial solution
        partial(0) = first
        partial(a.rows-1) = last
        var best = Array.tabulate(a.rows){i => i}
        var bestVal = a.getError
        val stack = new Stack[Node]()

        stack.push(Node(partial, remaining, 0, 1))

        while(!stack.isEmpty){
            val e = stack.pop()

            if(e.isComplete && bestVal > e.getError){
                // if the solution is complete and is better than the previous ones
                for(i <- 0 until a.rows){
                    best(i) = e.partial(i)
                }
                upper = Math.min(upper, bestVal)
                bestVal = e.getError
            }
            else if(e.hasNext){
                stack.push(e)
                // generate next child of e
                val child = e.getNext

                if(child.getLowerBound < upper){
                    // keep this branch only if it is worth exploring it
                    stack.push(child)
                }

            }
        }

        (bestVal, best)
    }

    /**
    *   @param r row of the point
    *   @param c column of the point
    *   @param up row above r
    *   @param down row below r
    *   @return the convolution of point at position (r,c) supposing that
    *           the rows above and below it are respectively up and down
    *   @note Warning: only works for three by three kernels
    */
    private def convoluteSinglePoint(r:Int, c:Int, up: Int, down: Int):Double = {
      var sum = 0.0

      var col = 0
      var begin = c - kernel.cols/2
      while(col < kernel.cols){
          sum += a.getWithPadding(up, begin)*kernel(0,col)
          sum += a.getWithPadding(r, begin)*kernel(1,col)
          sum += a.getWithPadding(down, begin)*kernel(2,col)
          col += 1
          begin += 1
      }
      sum
    }

    /**
    *   @param row row for which we want to compute the error
    *   @param rowUp row above row
    *   @param rowDown row below row
    *   @return computes the sum of the errors of the elements of this row
    */
    private def computeErrorRow(row: Int, rowUp: Int, rowDown: Int): Double = {
      var c = 0
      var sumTot = 0.0
      while(c < a.cols){
          if(a(row,c) != 0){
              val sum = convoluteSinglePoint(row,c,rowUp,rowDown)
              sumTot += a.computeSingleError(a(row,c), sum)
          }
          c += 1
      }

      sumTot
    }

    /**
    *   @param partial partial solution
    *   @param up row above r
    *   @param down row below r
    *   @return the sum of the errors of the elements of this row
    *           If it is not in the cache, compute it and then add it to the cache
    */
    private def getErrorRow(partial: Array[Int], r: Int, up: Int, down: Int): Double = {

        val res =   if(up < down) cache.get(s"$r-$up-$down")
                    else cache.get(s"$r-$down-$up")

        res match {
            case Some(x) => x // in the cache
            case None => {
                // not in the cache
                val v = computeErrorRow(r, up, down)
                if(up < down) cache += (s"$r-$up-$down" -> v)
                else cache += (s"$r-$down-$up" -> v)
                v
            }
        }
    }

    /**
    *   @param partial partial solution
    *   @param r row we want to convert
    *   @return if r is an indice of partial, returns the value of partial at this index
    *           otherwise returns r
    */
    private def convert(partial: Array[Int], r: Int) = {
        if(0 <= r && r <= partial.size-1) partial(r) else r
    }

    /**
    *   @param oldError error of the previous partial solution
    *   @param partial partial solution
    *   @param i row that was added to the partial solution
    *   @return new errro
    */
    private def updateError(oldError: Double, partial: Array[Int], i: Int): Double = {
        var newError = oldError

        if(i == 0) newError // first row, we cannot compute the error yet
        else newError += getErrorRow(partial, convert(partial,i-1), convert(partial,i-2), convert(partial,i))

        if(i==a.rows-2) newError += getErrorRow(partial, convert(partial,i), convert(partial,i-1), convert(partial,i+1)) + getErrorRow(partial, convert(partial,i+1), convert(partial,i) , convert(partial,a.rows))

        newError
    }

    /**
    *   @param partial The partial solution
    *   @param indices The indices of the remaning rows
    *   @param up index of the last added row of partial
    *   @param down index of the very last row of partial
    *   @return a lower bound for the number of 0-1 transitions
    *       Thus the number of 1-0 transitions with the hypothesis that we can
    *       use the same row several time.
    *       For each row, computes the minimal number of transitions that we can
    *       have if we put the "best" rows next to it
    */
    private def lowerBound(partial:Array[Int], indices: Array[Int], up: Int, down: Int) = {
        var sum = 0.0
        var bestUp = Double.MaxValue
        var bestDown = Double.MaxValue
        for(i <- 0 until indices.size){
            // best number of transitions between up (resp. down) and the row below (resp. above)
            bestUp = Math.min(bestUp,getErrorRow(indices, convert(partial,up), convert(partial,up-1), indices(i)))
            bestDown = Math.min(bestDown,getErrorRow(indices, convert(partial,down), indices(i), convert(partial,down+1)))

            var best = if(indices.size==1) getErrorRow(indices, indices(i), convert(partial,up), convert(partial,down)) else Double.MaxValue

            for(r <- 0 until indices.size; s <- r+1 until indices.size; if(i!=r && i!=s)){
                best = Math.min(best, getErrorRow(indices, indices(i), indices(r), indices(s)))
            }

            for(r <- 0 until indices.size; if(i!=r)){
                best = Math.min(best, getErrorRow(indices, indices(i), convert(partial,up), indices(r)))
            }

            for(r <- 0 until indices.size; if(i!=r)){
                best = Math.min(best, getErrorRow(indices, indices(i), indices(r), convert(partial,down)))
            }

            sum += best
        }

        sum + bestUp + bestDown
    }
}
