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

class TestCategoricalMatrix extends FlatSpec with Matchers with MatrixHelpers{

	val catMatrix = DenseMatrix(
	    (2.0, 0.0, 0.0, 2.0, 2.0, 0.0, 0.0), // 0
	    (0.0, 2.0, 0.0, 2.0, 2.0, 2.0, 2.0), // 1
	    (0.0, 2.0, 2.0, 0.0, 0.0, 2.0, 0.0), // 2
	    (2.0, 0.0, 2.0, 2.0, 2.0, 0.0, 2.0)  // 3
  	)

  	val catMatrixColor = DenseMatrix(
	    (0.0, 1.0, 1.0, 0.0, 0.0, 1.0, 1.0), // 0
	    (1.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0), // 1
	    (1.0, 0.0, 0.0, 1.0, 1.0, 0.0, 1.0), // 2
	    (0.0, 1.0, 0.0, 0.0, 0.0, 1.0, 0.0)  // 3
  	)

  	val catMatrix0 = DenseMatrix(
	    (1.0, 0.0, 0.0, 1.0, 1.0, 0.0, 0.0), // 0
	    (0.0, 1.0, 0.0, 1.0, 1.0, 1.0, 1.0), // 1
	    (0.0, 1.0, 1.0, 0.0, 0.0, 1.0, 0.0), // 2
	    (1.0, 0.0, 1.0, 1.0, 1.0, 0.0, 1.0)  // 3
  	)

  	val catMatrix1 = DenseMatrix(
	    (0.0, 1.0, 1.0, 0.0, 0.0, 1.0, 1.0), // 0
	    (1.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0), // 1
	    (1.0, 0.0, 0.0, 1.0, 1.0, 0.0, 1.0), // 2
	    (0.0, 1.0, 0.0, 0.0, 0.0, 1.0, 0.0)  // 3
  	)

  	val delta = 1e-2

	val convolutionSettings = new ConvolutionSettings(Kernels.gaussianBlurSmooth(3,3), padding = Padding.ZerosAndOnes)

	val catMatrixM:MatrixMoves = BinarySparseMatrix(catMatrix, convolutionSettings = convolutionSettings.copy)

	"A categorical matrix of 2 categories" should "be clonable" in {
		val x = catMatrix.toSequentialCategories

	    val theCopy = catMatrixM.copy

		theCopy.select(0)
		theCopy should equal (catMatrix0)

		theCopy.select(1)
		theCopy should equal (catMatrix1)

	    theCopy.getRows should contain theSameElementsInOrderAs Array.range(0,catMatrix.rows)
	    theCopy.getCols should contain theSameElementsInOrderAs Array.range(0,catMatrix.cols)
	}


	it should "be split in 2 binary matrices" in {
		val theM = catMatrixM.copy
		theM.isCat should be (true)
		theM.nBinaryMatrices should be (2)

		theM.select(0)
		theM should equal (catMatrix0)

		theM.select(1)
		theM should equal (catMatrix1)

		theM.getError should equal (catMatrix0.computeError(convolutionSettings) + catMatrix1.computeError(convolutionSettings) +- delta)

		theM.getMatrix should equal (catMatrixColor)
	}

	it should "transpose correctly" in {
		val theM = catMatrixM.copy
		theM.t

		theM.select(0)
		theM should equal (catMatrix0.t)

		theM.select(1)
		theM should equal (catMatrix1.t)

		theM.getError should equal (catMatrix0.t.computeError(convolutionSettings.t) + catMatrix1.t.computeError(convolutionSettings.t) +- delta)

		theM.getMatrix should equal (catMatrixColor.t)

		theM.t

		theM.select(0)
		theM should equal (catMatrix0)

		theM.select(1)
		theM should equal (catMatrix1)

		theM.getError should equal (catMatrix0.computeError(convolutionSettings) + catMatrix1.computeError(convolutionSettings) +- delta)

		theM.getMatrix should equal (catMatrixColor)
	}

	it should "have the correct error after a swap" in {
	  	val catMatrixColorSwap = DenseMatrix(
		    (0.0, 1.0, 1.0, 0.0, 0.0, 1.0, 1.0), // 0
		    (1.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0), // 1
		    (0.0, 1.0, 0.0, 0.0, 0.0, 1.0, 0.0), // 3
		    (1.0, 0.0, 0.0, 1.0, 1.0, 0.0, 1.0)  // 2
	  	)

	  	val catMatrix0Swap = DenseMatrix(
		    (1.0, 0.0, 0.0, 1.0, 1.0, 0.0, 0.0), // 0
		    (0.0, 1.0, 0.0, 1.0, 1.0, 1.0, 1.0), // 1
		    (1.0, 0.0, 1.0, 1.0, 1.0, 0.0, 1.0), // 3
		    (0.0, 1.0, 1.0, 0.0, 0.0, 1.0, 0.0)  // 2
	  	)

	  	val catMatrix1Swap = DenseMatrix(
		    (0.0, 1.0, 1.0, 0.0, 0.0, 1.0, 1.0), // 0
		    (1.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0), // 1
		    (0.0, 1.0, 0.0, 0.0, 0.0, 1.0, 0.0), // 3
		    (1.0, 0.0, 0.0, 1.0, 1.0, 0.0, 1.0)  // 2
	  	)

	  	val theM = catMatrixM.copy

	  	val reverseMove = theM.swap(2,3)

		theM.select(0)
		theM should equal (catMatrix0Swap)

		theM.select(1)
		theM should equal (catMatrix1Swap)

		theM.getMatrix should equal (catMatrixColorSwap)

		theM.getError should equal (catMatrix0Swap.computeError(convolutionSettings) + catMatrix1Swap.computeError(convolutionSettings) +- delta)

		reverseMove.reverseTheMove

		theM.select(0)
		theM should equal (catMatrix0)

		theM.select(1)
		theM should equal (catMatrix1)

		theM.getError should equal (catMatrix0.computeError(convolutionSettings) + catMatrix1.computeError(convolutionSettings) +- delta)

		theM.getMatrix should equal (catMatrixColor)
	}

	it should "work correcty after a reverse" in {
	  	val catMatrixColorSwap = DenseMatrix(
		    (1.0, 0.0, 0.0, 1.0, 1.0, 0.0, 1.0), // 2
		    (1.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0), // 1
		    (0.0, 1.0, 1.0, 0.0, 0.0, 1.0, 1.0), // 0
		    (0.0, 1.0, 0.0, 0.0, 0.0, 1.0, 0.0)  // 3
	  	)

	  	val catMatrix0Swap = DenseMatrix(
		    (0.0, 1.0, 1.0, 0.0, 0.0, 1.0, 0.0), // 2
		    (0.0, 1.0, 0.0, 1.0, 1.0, 1.0, 1.0), // 1
		    (1.0, 0.0, 0.0, 1.0, 1.0, 0.0, 0.0), // 0
		    (1.0, 0.0, 1.0, 1.0, 1.0, 0.0, 1.0)  // 3
	  	)

	  	val catMatrix1Swap = DenseMatrix(
		    (1.0, 0.0, 0.0, 1.0, 1.0, 0.0, 1.0), // 2
		    (1.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0), // 1
		    (0.0, 1.0, 1.0, 0.0, 0.0, 1.0, 1.0), // 0
		    (0.0, 1.0, 0.0, 0.0, 0.0, 1.0, 0.0)  // 3
	  	)

	  	val theM = catMatrixM.copy

	  	val reverseMove = theM.reverse(0,2)

		theM.select(0)
		theM should equal (catMatrix0Swap)

		theM.select(1)
		theM should equal (catMatrix1Swap)

		theM.getError should equal (catMatrix0Swap.computeError(convolutionSettings) + catMatrix1Swap.computeError(convolutionSettings) +- delta)
		theM.getMatrix should equal (catMatrixColorSwap)

		reverseMove.reverseTheMove

		theM.select(0)
		theM should equal (catMatrix0)

		theM.select(1)
		theM should equal (catMatrix1)

		theM.getError should equal (catMatrix0.computeError(convolutionSettings) + catMatrix1.computeError(convolutionSettings) +- delta)

		theM.getMatrix should equal (catMatrixColor)
	}


	it should "work correcty after a relocate" in {
	  	val catMatrixColorSwap = DenseMatrix(
		    (0.0, 1.0, 0.0, 0.0, 0.0, 1.0, 0.0), // 3
		    (0.0, 1.0, 1.0, 0.0, 0.0, 1.0, 1.0), // 0
		    (1.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0), // 1
		    (1.0, 0.0, 0.0, 1.0, 1.0, 0.0, 1.0)  // 2
	  	)

	  	val catMatrix0Swap = DenseMatrix(
		    (1.0, 0.0, 1.0, 1.0, 1.0, 0.0, 1.0), // 3
		    (1.0, 0.0, 0.0, 1.0, 1.0, 0.0, 0.0), // 0
		    (0.0, 1.0, 0.0, 1.0, 1.0, 1.0, 1.0), // 1
		    (0.0, 1.0, 1.0, 0.0, 0.0, 1.0, 0.0)  // 2
	  	)

	  	val catMatrix1Swap = DenseMatrix(
		    (0.0, 1.0, 0.0, 0.0, 0.0, 1.0, 0.0), // 3
		    (0.0, 1.0, 1.0, 0.0, 0.0, 1.0, 1.0), // 0
		    (1.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0), // 1
		    (1.0, 0.0, 0.0, 1.0, 1.0, 0.0, 1.0)  // 2
	  	)

	  	val reverseMove = catMatrixM.relocate(0,2,1)

		catMatrixM.select(0)
		catMatrixM should equal (catMatrix0Swap)

		catMatrixM.select(1)
		catMatrixM should equal (catMatrix1Swap)

		catMatrixM.getError should equal (catMatrix0Swap.computeError(convolutionSettings) + catMatrix1Swap.computeError(convolutionSettings) +- delta)

		catMatrixM.getMatrix should equal (catMatrixColorSwap)


		reverseMove.reverseTheMove

		catMatrixM.select(0)
		catMatrixM should equal (catMatrix0)

		catMatrixM.select(1)
		catMatrixM should equal (catMatrix1)

		catMatrixM.getError should equal (catMatrix0.computeError(convolutionSettings) + catMatrix1.computeError(convolutionSettings) +- delta)

		catMatrixM.getMatrix should equal (catMatrixColor)
	}

}
