import org.scalatest._

import thesis.dataLoading._
import thesis.matrixTypes._
import thesis.orderings._
import thesis.rectangles._
import thesis.utils._

import thesis._
import breeze.linalg._, eigSym.EigSym
import breeze.plot._
import breeze.stats.distributions._
import breeze.stats._

import breeze.numerics._

class TestBruteForce extends FlatSpec with Matchers with MatrixHelpers{

  val banded = DenseMatrix(
    (0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0),  // 7
    (0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 1.0),  // 6
    (0.0, 0.0, 0.0, 0.0, 1.0, 1.0, 0.0), // 5
    (0.0, 0.0, 0.0, 1.0, 1.0, 1.0, 0.0), // 4
    (0.0, 0.0, 1.0, 1.0, 1.0, 0.0, 0.0), // 3
    (0.0, 1.0, 1.0, 1.0, 0.0, 0.0, 0.0), // 2
    (1.0, 1.0, 1.0, 0.0, 0.0, 0.0, 0.0), // 1
    (1.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0) // 0

  )

	val bandedShuffledRows = DenseMatrix(
    (0.0, 0.0, 0.0, 1.0, 1.0, 1.0, 0.0), // 4 0
    (1.0, 1.0, 1.0, 0.0, 0.0, 0.0, 0.0), // 1 1
    (0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 1.0), // 6 2
    (1.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0), // 0 3
    (0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0), // 7 4
    (0.0, 1.0, 1.0, 1.0, 0.0, 0.0, 0.0), // 2 5
    (0.0, 0.0, 0.0, 0.0, 1.0, 1.0, 0.0), // 5 6
    (0.0, 0.0, 1.0, 1.0, 1.0, 0.0, 0.0)  // 3 7
	)


  val delta = 1e-2
  val convolutionSettings = ConvolutionSettings(Kernels.gaussianBlur, ErrorType.Square, padding = Padding.ZerosAndOnes)

  val bandedM:MatrixMoves = BinarySparseMatrix(banded, convolutionSettings=convolutionSettings.copy) // Change to SparseMatrix or MatrixPermutations here
  val bandedShuffledRowsM:MatrixMoves = BinarySparseMatrix(bandedShuffledRows, convolutionSettings=convolutionSettings.copy)

  val bruteForce = new BruteForceImproved()

  "Brute force" should "work on an unshuffled matrix" in {
    val ordered = bruteForce.changeOrdering(bandedM.copy)

    ordered.getRows should contain theSameElementsInOrderAs Array(0,1,2,3,4,5,6,7)

    ordered should equal (banded)

    ordered.getError should equal (banded.computeError(convolutionSettings) +- delta)

  }

  it should "work on a row shuffled matrix" in {
    val ordered = bruteForce.changeOrdering(bandedShuffledRowsM.copy)

    ordered.getRows should contain theSameElementsInOrderAs Array(4, 2, 6, 0, 7, 5, 1, 3)

    ordered should equal (banded)

    ordered.getError should equal (banded.computeError(convolutionSettings) +- delta)
  }


}
