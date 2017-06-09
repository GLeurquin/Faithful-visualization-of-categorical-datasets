package thesis.orderings
import thesis.matrixTypes._
import thesis.utils._

/** Uses heatmap3 from R to reorder the matrix
*	@param folder The folder in which to save the dendrogram
*/
class Heatmap(folder:String = "") extends MatrixOrdering {
	override def toString = "Heatmap"

	/**
	*	@inheritdoc
	*/
	override def changeOrdering(m:MatrixMoves):MatrixMoves = {
		import sys.process._
		import scala.language.postfixOps

		val filename = folder + m.desc

		def cleanup() = {
			(new java.io.File(s"${filename}rowInd.order")).delete()
			(new java.io.File(s"${filename}colInd.order")).delete()
			(new java.io.File(s"$filename.rformat")).delete()
		}


		m.writeMatrixAsRInput(filename) // Start by writing this matrix to a r file
		// Run the R program to process this file:
		val result = s"Rscript src/main/R/makeHeatmap.r $filename ${if(m.isCat) 1 else 0}" ! // This creates a .png with the heatmap

		if(result != 0){
			println("An error occured...")
			cleanup()
			return m
		}

		val rowIndices = scala.io.Source.fromFile(s"${filename}rowInd.order").getLines.drop(1).mkString("\n").split("\n").map(_.toInt - 1)
		val colIndices = scala.io.Source.fromFile(s"${filename}colInd.order").getLines.drop(1).mkString("\n").split("\n").map(_.toInt - 1)

		assert(rowIndices.length == m.rows)
		assert(rowIndices.forall(_ < m.rows))
		assert(colIndices.length == m.cols)
		assert(colIndices.forall(_ < m.cols))

		// Row indices and colIndices still have to be passed through realMapped because they are not a mapping of anything

		m.permute(rowIndices.reverse.map(r => m.mappedRows(r)), mapping = false)
		m.t
		m.permute(colIndices.reverse.map(c => m.mappedCols(c)), mapping = false)
		m.t

		cleanup()

		m
	}
}
