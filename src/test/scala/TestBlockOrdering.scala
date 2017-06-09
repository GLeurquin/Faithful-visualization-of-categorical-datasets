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

class TestBlockOrdering extends FlatSpec with Matchers with MatrixHelpers{

  // (1.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0),
  // (1.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0),
  // (0.0, 1.0, 1.0, 1.0, 0.0, 0.0, 0.0),
  // (0.0, 1.0, 1.0, 1.0, 1.0, 0.0, 0.0),
  // (0.0, 0.0, 0.0, 1.0, 1.0, 1.0, 0.0),
  // (0.0, 0.0, 0.0, 0.0, 1.0, 1.0, 0.0),
  // (0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 1.0)

  val originalMatrixOdd = DenseMatrix(
    (1.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0),
    (0.0, 1.0, 1.0, 1.0, 0.0, 0.0, 0.0),
    (0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 1.0),
    (0.0, 1.0, 1.0, 1.0, 1.0, 0.0, 0.0),
    (1.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0),
    (0.0, 0.0, 0.0, 0.0, 1.0, 1.0, 0.0),
    (0.0, 0.0, 0.0, 1.0, 1.0, 1.0, 0.0)
  )

	val squashedOdd = DenseMatrix( // for 2
    (1.0, 1.0, 1.0, 1.0, 0.0, 0.0, 0.0),
    (0.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0),
    (1.0, 1.0, 0.0, 0.0, 1.0, 1.0, 0.0),
    (0.0, 0.0, 0.0, 1.0, 1.0, 1.0, 0.0)
	)

	val originalMatrixEven = DenseMatrix(
    (1.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0),
    (0.0, 1.0, 1.0, 1.0, 0.0, 0.0, 0.0),
    (0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 1.0),
    (0.0, 1.0, 1.0, 1.0, 1.0, 0.0, 0.0),
    (1.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0),
    (0.0, 0.0, 0.0, 0.0, 1.0, 1.0, 0.0)
  )

  val squashedEven = DenseMatrix( // for 2
    (1.0, 1.0, 1.0, 1.0, 0.0, 0.0, 0.0),
    (0.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0),
    (1.0, 1.0, 0.0, 0.0, 1.0, 1.0, 0.0)
  )

  val delta = 1e-2
	val convolutionSettings = ConvolutionSettings(Kernels.gaussianBlurSmooth(3,3), ErrorType.Abs, padding = Padding.ZerosAndOnes)

  val originalMatrixOddMat:MatrixMoves = BinarySparseMatrix(originalMatrixOdd, convolutionSettings=convolutionSettings.copy) // Change to SparseMatrix or MatrixPermutations here
  val squashedOddMat:MatrixMoves = BinarySparseMatrix(squashedOdd, convolutionSettings=convolutionSettings.copy) // Change to SparseMatrix or MatrixPermutations here
  val originalMatrixEvenMat:MatrixMoves = BinarySparseMatrix(originalMatrixEven, convolutionSettings=convolutionSettings.copy) // Change to SparseMatrix or MatrixPermutations here
  val squashedEvenMat:MatrixMoves = BinarySparseMatrix(squashedEven, convolutionSettings=convolutionSettings.copy) // Change to SparseMatrix or MatrixPermutations here

  // Favors 1.0
  def squash(a:Array[Double]):Double = {
    val nOne = a.count(_ == 1.0)
    val nZeros = a.length - nOne
    if(nZeros > nOne) 0.0
    else 1.0
  }

  val bruteForceOrdering = new BruteForceImproved()

  val blockOrdering = new BlockOrdering(nBlocks=3, squash(_), bruteForceOrdering)

  "Block ordering for even blocks" should "work correctly" in {
    val newOrdering = blockOrdering.changeOrdering(originalMatrixEvenMat.copy)
    val evenSquashed = bruteForceOrdering.changeOrdering(squashedEvenMat.copy)

    evenSquashed.getRows should contain theSameElementsInOrderAs Array(0,1,2)

    evenSquashed should equal{
      DenseMatrix(
        (1.0, 1.0, 1.0, 1.0, 0.0, 0.0, 0.0), // 0
        (0.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0), // 1
        (1.0, 1.0, 0.0, 0.0, 1.0, 1.0, 0.0)  // 2

      )
    }

    newOrdering.getRows should contain theSameElementsInOrderAs Array(0,1,2,3,4,5)

    val x2 = DenseMatrix(
        (1.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0), // 0 --> 0
        (0.0, 1.0, 1.0, 1.0, 0.0, 0.0, 0.0), // 1 --> 0
      (0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 1.0), // 2 --> 1
      (0.0, 1.0, 1.0, 1.0, 1.0, 0.0, 0.0),  // 3 --> 1
      (1.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0), // 4 --> 2
      (0.0, 0.0, 0.0, 0.0, 1.0, 1.0, 0.0) // 5 --> 2
    )

    newOrdering should equal (x2)

    newOrdering.getError should equal (x2.computeError(convolutionSettings) +- delta)
  }

  "Block ordering for odd blocks" should "work correctly" in {
    val newOrdering = blockOrdering.changeOrdering(originalMatrixOddMat.copy)
    val oddSquashed = bruteForceOrdering.changeOrdering(squashedOddMat.copy)

    val oddSquashedCopy = oddSquashed.copy
    oddSquashedCopy.permute(Array(0,1,2,3))

    oddSquashed.getRows should contain theSameElementsInOrderAs Array(0,2,1,3)

    oddSquashed should equal{
      DenseMatrix(
          (1.0, 1.0, 1.0, 1.0, 0.0, 0.0, 0.0), // 0
        (1.0, 1.0, 0.0, 0.0, 1.0, 1.0, 0.0), // 2
        (0.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0),  // 1
        (0.0, 0.0, 0.0, 1.0, 1.0, 1.0, 0.0) // 3
      )
    }

    newOrdering.getRows should contain theSameElementsInOrderAs Array(0,1,4,5,2,3,6)

    val x2 = DenseMatrix(
        (1.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0), // 0 --> 0
        (0.0, 1.0, 1.0, 1.0, 0.0, 0.0, 0.0), // 1 --> 0
        (1.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0), // 4 --> 2
        (0.0, 0.0, 0.0, 0.0, 1.0, 1.0, 0.0), // 5 --> 2
        (0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 1.0), // 2 --> 1
        (0.0, 1.0, 1.0, 1.0, 1.0, 0.0, 0.0),  // 3 --> 1
        (0.0, 0.0, 0.0, 1.0, 1.0, 1.0, 0.0)  // 6 --> 3
    )

    newOrdering should equal (x2)

    newOrdering.getError should equal (x2.computeError(convolutionSettings) +- delta)
  }

}
