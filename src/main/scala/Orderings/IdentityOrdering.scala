package thesis.orderings
import thesis.matrixTypes._
import thesis.utils._

/** Ordering that just returns the same matrix
*/
class Identity extends MatrixOrdering {
	override def toString = "Identity"

	/**
	*	@inheritdoc
	*/
	override def changeOrdering(m:MatrixMoves) = m
}