package thesis.orderings
import thesis.matrixTypes._
import thesis.utils._

/**
 * Reorder the matrix with a branch and bound brute force ordering to find
 * a perfect solution
 */
class BruteForceImproved() extends MatrixOrdering{
  override def toString:String = "Brute Force"

  private def findOrdering(a:MatrixMoves): Array[Int] = {
      val bAb = new BranchAndBoundImproved(a)
      bAb.run
  }

  /**
  *   @inheritdoc
  */
  override def changeOrdering(a:MatrixMoves) = {
      if(a.isCat) throw new IllegalArgumentException("BruteForce ordering only works for binary data")
    require(a.rows <= 20, s"You messed up the size of the brute force matrix: ${a.rows}")
    // change the settings to make them compatible with the branch and bound
    val backUpSettings = a.getConvolutionSettings
    a.setConvolutionSettings(ConvolutionSettings(Kernels.gaussianBlurSmooth(3,3), ErrorType.Abs, Padding.Zero))
    var rowsOrder = findOrdering(a)
    a.permute(rowsOrder)
    a.setConvolutionSettings(backUpSettings) // recover the settings
    a
  }
}
