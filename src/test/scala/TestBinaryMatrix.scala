import org.scalatest._


import thesis.dataLoading._
import thesis.matrixTypes._
import thesis.orderings._
import thesis.rectangles._
import thesis.utils._


import breeze.linalg._, eigSym.EigSym
import breeze.plot._
import breeze.stats.distributions._
import breeze.stats._

import breeze.numerics._

class TestBinaryMatrix extends FlatSpec with Matchers with MatrixHelpers{

  /**
  *   Contains t for
  *   Rows : 1 to 3
  *   Cols : 2 to 5
  */
  val containingT = DenseMatrix(
    (1.0, 0.0, 0.0, 1.0, 1.0, 0.0, 0.0),
    (1.0, 1.0, 0.0, 1.0, 1.0, 1.0, 1.0),
    (0.0, 1.0, 1.0, 0.0, 0.0, 1.0, 0.0),
    (1.0, 0.0, 1.0, 1.0, 1.0, 0.0, 1.0)
  )

	val t = DenseMatrix(
		(0.0, 1.0, 1.0, 1.0),
		(1.0, 0.0, 0.0, 1.0),
		(1.0, 1.0, 1.0, 0.0)
	)

	val bigTable = DenseMatrix(
		(0.0, 1.0, 1.0, 1.0),
		(1.0, 0.0, 0.0, 1.0),
		(1.0, 1.0, 1.0, 0.0),
		(0.0, 1.0, 1.0, 1.0),
		(1.0, 0.0, 1.0, 1.0),
		(1.0, 1.0, 0.0, 1.0),
		(0.0, 1.0, 1.0, 0.0)
	)

	val small = DenseMatrix(
		(1.0,0.0, 0.0, 0.0),
		(1.0, 0.0, 1.0, 0.0),
		(0.0, 1.0, 1.0, 0.0),
		(1.0, 0.0, 1.0, 0.0)
	)

  val delta = 1e-2
	val convolutionSettings = ConvolutionSettings(Kernels.gaussianBlurSmooth(3,3), padding=Padding.ZerosAndOnes)

	val ordering:MatrixMoves = BinarySparseMatrix(t, convolutionSettings=convolutionSettings.copy) // Change to SparseMatrix or MatrixPermutations here
  val myContainingT:MatrixMoves = BinarySparseMatrix(containingT, convolutionSettings=convolutionSettings.copy) // Change to SparseMatrix or MatrixPermutations here
	val myBigTable:MatrixMoves = BinarySparseMatrix(bigTable, convolutionSettings=convolutionSettings.copy) // and here, depending of what you want to test
	val mySmall:MatrixMoves = BinarySparseMatrix(small, convolutionSettings=convolutionSettings.copy)

	val theOrder = Array(2, 1, 0)

  "A copy" should "work correctly" in {
    val theCopy = ordering.copy
    theCopy should equal (t)
    theCopy.getRows should contain theSameElementsInOrderAs Array.range(0,t.rows)
    theCopy.getCols should contain theSameElementsInOrderAs Array.range(0,t.cols)
  }

  val newOrdering = ordering.copy
  newOrdering.permute(theOrder)

  val t2 = DenseMatrix(
    (1.0, 1.0, 1.0, 0.0),
    (1.0, 0.0, 0.0, 1.0),
    (0.0, 1.0, 1.0, 1.0)
  )

  "A permutation" should "give the correct matrix" in {
    newOrdering should equal (t2)
  }

  it should "give the correct updated convolution" in {
    val theCopy = newOrdering.copy
    t2.convolute(convolutionSettings) should equal (theCopy.getConvolution)
    t2.t.convolute(convolutionSettings.t) should equal (theCopy.t.getConvolution)
    theCopy.t
  }

  it should "give the correct updated error" in {
    val theCopy = newOrdering.copy
    theCopy.getError should equal (t2.computeError(convolutionSettings) +- delta)
    theCopy.t.getError should equal (t2.t.computeError(convolutionSettings.t) +- delta)
    theCopy.t
  }

  it should "have the right row positions" in {
    newOrdering.getRows should contain theSameElementsInOrderAs theOrder
  }

  it should "have the right col positions" in {
    newOrdering.getCols should contain theSameElementsInOrderAs Array.range(0,t.cols)
  }

  it should "give the right tips" in {
    newOrdering.tipX(0) should be ("0")
    newOrdering.tipX(1) should be ("1")
    newOrdering.tipX(2) should be ("2")
    newOrdering.tipX(3) should be ("3")

    newOrdering.tipY(0) should be ("2")
    newOrdering.tipY(1) should be ("1")
    newOrdering.tipY(2) should be ("0")
  }

  val t3 = DenseMatrix(
    (0.0, 1.0, 1.0, 1.0),
    (1.0, 1.0, 1.0, 0.0),
    (1.0, 0.0, 0.0, 1.0)
  )

  it should "give the correct matrix when permuting it again" in {
    val theCopy = newOrdering.copy
    theCopy.permute(Array(2,0,1))
    theCopy should equal (t3)
    theCopy should equal (t3)

    theCopy.getConvolution should equal (t3.convolute(convolutionSettings))

    theCopy.getError should equal (t3.computeError(convolutionSettings) +- delta)

    theCopy.getRows should contain theSameElementsInOrderAs Array(0,2,1)

    theCopy.getCols should contain theSameElementsInOrderAs Array.range(0,t.cols)

    theCopy.tipX(0) should be ("0")
    theCopy.tipX(1) should be ("1")
    theCopy.tipX(2) should be ("2")
    theCopy.tipX(3) should be ("3")

    theCopy.tipY(0) should be ("0")
    theCopy.tipY(1) should be ("2")
    theCopy.tipY(2) should be ("1")

    theCopy.isTranspose should be (false)
  }

  it should "not have modified the original ordering" in {
    ordering.equals(t) should be (true)
  }

  it should "give the correct matrix when the order is applied to the columns" in {
    val colOrder = Array(2,1,3,0)
    val theCopy = newOrdering.copy
    theCopy.permute(Array(2,0,1))
    theCopy.t.permute(colOrder)

    theCopy.getRows should contain theSameElementsInOrderAs colOrder

    theCopy should equal (DenseMatrix(
      (1.0, 1.0, 0.0),
      (1.0, 1.0, 0.0),
      (1.0, 0.0, 1.0),
      (0.0, 1.0, 1.0)
    ))

    theCopy.t should equal (DenseMatrix(
      (1.0,   1.0,  1.0,  0.0),
      (1.0,   1.0,  0.0,  1.0),
      (0.0,   0.0,  1.0,  1.0)
    ))

  }

  "Submatrix" should "give the correct sub matrix and convolution, unapply" in {
    /**
    *   the matrix "containingT" Contains t for
    *   Rows : 1 to 3
    *   Cols : 2 to 5
    */

    val myContainingTCopy = myContainingT.copy

    myContainingTCopy.isFullMatrix should be (true)

    myContainingTCopy.subMatrix((1 to 3), (2 to 5))

    myContainingTCopy.isFullMatrix should be (false)

    myContainingTCopy should equal (t)

    myContainingTCopy.getRows should contain theSameElementsInOrderAs Array(1,2,3)
    myContainingTCopy.getCols should contain theSameElementsInOrderAs Array(2,3,4,5)

    // myContainingTCopy.getError should equal (t.computeError(convolutionSettings.t) +- delta)

    myContainingTCopy.swap(1,2)

    val xA = DenseMatrix(
      (0.0, 1.0, 1.0, 1.0),
      (1.0, 1.0, 1.0, 0.0),
      (1.0, 0.0, 0.0, 1.0)
     )

    myContainingTCopy.getRows should contain theSameElementsInOrderAs Array(1,3,2)

    myContainingTCopy should equal (xA)

    // myContainingTCopy.getError should equal (xA.computeError(convolutionSettings) +- delta)

    myContainingTCopy.t

    val xB = DenseMatrix(
      (0.0,  1.0,  1.0),
      (1.0,  1.0,  0.0),
      (1.0,  1.0,  0.0),
      (1.0,  0.0,  1.0)
    )
    myContainingTCopy should equal (xB)

    myContainingTCopy.getCols should contain theSameElementsInOrderAs Array(1,3,2)
    myContainingTCopy.getRows should contain theSameElementsInOrderAs Array(2,3,4,5)

    myContainingTCopy.swap(2,3)

    val xC = DenseMatrix(
      (0.0,  1.0,  1.0),
      (1.0,  1.0,  0.0),
      (1.0,  0.0,  1.0),
      (1.0,  1.0,  0.0)
    )

    myContainingTCopy should equal (xC)

    myContainingTCopy.getCols should contain theSameElementsInOrderAs Array(1,3,2)
    myContainingTCopy.getRows should contain theSameElementsInOrderAs Array(2,3,5,4)

    myContainingTCopy.insertSubmatrixNoChange()

    myContainingTCopy.getCols should contain theSameElementsInOrderAs Array(0,1,2,3)
    myContainingTCopy.getRows should contain theSameElementsInOrderAs Array(0,1,2,3,4,5,6)

    myContainingTCopy should equal (containingT.t)

    myContainingTCopy.t

    myContainingTCopy should equal (containingT)

    myContainingTCopy.getError should equal (containingT.computeError(convolutionSettings) +- delta)
  }


  it should "give the correct sub matrix and convolution, apply" in {
    /**
    *   the matrix "containingT" Contains t for
    *   Rows : 1 to 3
    *   Cols : 2 to 5
    */

    val myContainingTCopy = myContainingT.copy

    myContainingTCopy.subMatrix((1 to 3), (2 to 5))

    myContainingTCopy should equal (t)

    myContainingTCopy.getRows should contain theSameElementsInOrderAs Array(1,2,3)
    myContainingTCopy.getCols should contain theSameElementsInOrderAs Array(2,3,4,5)

    // myContainingTCopy.getError should equal (t.computeError(convolutionSettings) +- delta)

    myContainingTCopy.swap(1,2)

    myContainingTCopy.getRows should contain theSameElementsInOrderAs Array(1,3,2)

    val xA = DenseMatrix(
      (0.0, 1.0, 1.0, 1.0),
      (1.0, 1.0, 1.0, 0.0),
      (1.0, 0.0, 0.0, 1.0)
     )


    myContainingTCopy should equal (xA)

    // myContainingTCopy.getError should equal (xA.computeError(convolutionSettings) +- delta)

    myContainingTCopy.t


    val xB = DenseMatrix(
      (0.0, 1.0, 1.0),
      (1.0, 1.0, 0.0),
      (1.0, 1.0, 0.0),
      (1.0, 0.0, 1.0)
    )

    myContainingTCopy should equal (xB)

    myContainingTCopy.swap(0,1)

    myContainingTCopy.getRows should contain theSameElementsInOrderAs Array(3,2,4,5)

    val xC = DenseMatrix(
      (1.0, 1.0, 0.0),
      (0.0, 1.0, 1.0),
      (1.0, 1.0, 0.0),
      (1.0, 0.0, 1.0)
    )

    myContainingTCopy should equal (xC)

    // myContainingTCopy.getError should equal (xC.computeError(convolutionSettings) +- delta)

    myContainingTCopy.insertSubmatrix()

    /*
    * the matrix "containingT" Contains t for
    *   Rows : 1 to 3
    *   Cols : 2 to 5
    */

    // containingT
    //            x 0  x 1  x 2  x 3  x 4
    // (1.0, 0.0, 0.0, 1.0, 1.0, 0.0, 0.0),
    // (1.0, 1.0, 0.0, 1.0, 1.0, 1.0, 1.0), x 0
    // (0.0, 1.0, 1.0, 0.0, 0.0, 1.0, 0.0), x 1
    // (1.0, 0.0, 1.0, 1.0, 1.0, 0.0, 1.0)  x 2

    // containingT.swap(2,3)
    //            x 0  x 1  x 2  x 3  x 4
    // (1.0, 0.0, 0.0, 1.0, 1.0, 0.0, 0.0),
    // (1.0, 1.0, 0.0, 1.0, 1.0, 1.0, 1.0), x 0
    // (1.0, 0.0, 1.0, 1.0, 1.0, 0.0, 1.0), x 1
    // (0.0, 1.0, 1.0, 0.0, 0.0, 1.0, 0.0)  x 2

    // containingT.swap(2,3).t
    //       x 0  x 1  x 2
    // (1.0, 1.0, 1.0, 0.0),
    // (0.0, 1.0, 0.0, 1.0),
    // (0.0, 0.0, 1.0, 1.0), x 0
    // (1.0, 1.0, 1.0, 0.0), x 1
    // (1.0, 1.0, 1.0, 0.0), x 2
    // (0.0, 1.0, 0.0, 1.0), x 3
    // (0.0, 1.0, 1.0, 0.0)  x 4

    // containingT.swap(2,3).t.swap(2,3)
    val containingTWithMoves = DenseMatrix(
      (1.0, 1.0, 1.0, 0.0),
      (0.0, 1.0, 0.0, 1.0),
      (1.0, 1.0, 1.0, 0.0),
      (0.0, 0.0, 1.0, 1.0),
      (1.0, 1.0, 1.0, 0.0),
      (0.0, 1.0, 0.0, 1.0),
      (0.0, 1.0, 1.0, 0.0)
    )

    myContainingTCopy should equal (containingTWithMoves)

    myContainingTCopy.getError should equal (containingTWithMoves.computeError(convolutionSettings) +- delta)
  }


  "A swap" should "give the correct matrix when swapping rows and give the correct tips" in {
    val swapOrdering1 = ordering.copy

    swapOrdering1.getError should equal (t.computeError(convolutionSettings) +- delta)

    swapOrdering1.isFullMatrix should be (true)

    swapOrdering1.swap(1,2)

    val xA = DenseMatrix(
      (0.0, 1.0, 1.0, 1.0),
      (1.0, 1.0, 1.0, 0.0),
      (1.0, 0.0, 0.0, 1.0)
     )

    swapOrdering1 should equal (xA)

    swapOrdering1.getError should equal (xA.computeError(convolutionSettings) +- delta)

    swapOrdering1.swap(0, 1)

    val xB = DenseMatrix(
      (1.0, 1.0, 1.0, 0.0),
      (0.0, 1.0, 1.0, 1.0),
      (1.0, 0.0, 0.0, 1.0)
     )

    swapOrdering1 should equal (xB)

    swapOrdering1.getError should equal (xB.computeError(convolutionSettings) +- delta)

    swapOrdering1.tipX(0) should be ("0")
    swapOrdering1.tipX(1) should be ("1")
    swapOrdering1.tipX(2) should be ("2")
    swapOrdering1.tipX(3) should be ("3")

    swapOrdering1.tipY(0) should be ("2")
    swapOrdering1.tipY(1) should be ("0")
    swapOrdering1.tipY(2) should be ("1")

  }

  it should "give the correct matrix when swapping on columns" in {
    val swapOrdering = ordering.copy.t
    swapOrdering.swap(0,1)

    swapOrdering.isFullMatrix should be (true)
    swapOrdering.isTranspose should be (true)

    swapOrdering should equal (
      DenseMatrix(
        (1.0, 0.0, 1.0),
        (0.0, 1.0, 1.0),
        (1.0, 0.0, 1.0),
        (1.0, 1.0, 0.0)
     )
    )

    swapOrdering.t should equal (
      DenseMatrix(
        (1.0, 0.0, 1.0, 1.0),
        (0.0, 1.0, 0.0, 1.0),
        (1.0, 1.0, 1.0, 0.0)
     )
    )

    swapOrdering.isTranspose should be (false)

    ordering should equal (t)
  }

	"My swap" should "give the correct matrix when swapping rows and give the correct tips" in {
    val swapOrdering1 = mySmall.copy
    swapOrdering1.isFullMatrix should be (true)

		val small = DenseMatrix(
			(1.0,0.0, 0.0, 0.0),
			(1.0, 0.0, 1.0, 0.0),
			(0.0, 1.0, 1.0, 0.0),
			(1.0, 0.0, 1.0, 0.0)
		)

    swapOrdering1.getError should equal (swapOrdering1.getFullConvolutionError +- delta)

    swapOrdering1.swap(0,3)

    val xA = DenseMatrix(
			(1.0, 0.0, 1.0, 0.0),
			(1.0, 0.0, 1.0, 0.0),
			(0.0, 1.0, 1.0, 0.0),
			(1.0,0.0, 0.0, 0.0)
		)

    swapOrdering1 should equal (xA)

    swapOrdering1.getError should equal (swapOrdering1.getFullConvolutionError +- delta)
  }

  "A swap on a big array" should "give the correct matrix and convolution" in {
    val swapOrdering1 = myBigTable.copy
    val reverseSwap = swapOrdering1.swap(1,4)

    swapOrdering1.isFullMatrix should be (true)

    val xA = DenseMatrix(
      (0.0, 1.0, 1.0, 1.0),
      (1.0, 0.0, 1.0, 1.0),
      (1.0, 1.0, 1.0, 0.0),
      (0.0, 1.0, 1.0, 1.0),
      (1.0, 0.0, 0.0, 1.0),
      (1.0, 1.0, 0.0, 1.0),
      (0.0, 1.0, 1.0, 0.0)
     )

    swapOrdering1 should equal (xA)
    swapOrdering1.getError should equal (xA.computeError(convolutionSettings) +- delta)

    /**
    *   Reverse the move
    */
    reverseSwap.reverseTheMove

    // The matrix should be back to the original
    swapOrdering1 should equal (bigTable)

    swapOrdering1.getError should equal (bigTable.computeError(convolutionSettings) +- delta)

  }

   "An asymetric swap whith a.size > b.size on a big array" should "give the correct matrix and convolution" in {
    val swapOrdering1 = myBigTable.copy
    val reverseSwap = swapOrdering1.swap((0 until 2),(2 to 2))

    swapOrdering1.isFullMatrix should be (true)

    val xA = DenseMatrix(
        (1.0, 1.0, 1.0, 0.0),
        (0.0, 1.0, 1.0, 1.0),
        (1.0, 0.0, 0.0, 1.0),
        (0.0, 1.0, 1.0, 1.0),
        (1.0, 0.0, 1.0, 1.0),
        (1.0, 1.0, 0.0, 1.0),
        (0.0, 1.0, 1.0, 0.0)
     )

    swapOrdering1 should equal (xA)

    swapOrdering1.tipX(0) should be ("0")

    swapOrdering1.tipY(0) should be ("2")
    swapOrdering1.tipY(1) should be ("0")
    swapOrdering1.tipY(2) should be ("1")
    swapOrdering1.tipY(3) should be ("3")
    swapOrdering1.tipY(4) should be ("4")
    swapOrdering1.tipY(5) should be ("5")
    swapOrdering1.tipY(6) should be ("6")

    swapOrdering1.getError should equal (xA.computeError(convolutionSettings) +- delta)

    /**
    *   Reverse the move
    */
    reverseSwap.reverseTheMove

    val xB = DenseMatrix(
        (0.0, 1.0, 1.0, 1.0),
        (1.0, 0.0, 0.0, 1.0),
        (1.0, 1.0, 1.0, 0.0),
        (0.0, 1.0, 1.0, 1.0),
        (1.0, 0.0, 1.0, 1.0),
        (1.0, 1.0, 0.0, 1.0),
        (0.0, 1.0, 1.0, 0.0)
      )

    // The matrix should be back to the original
    swapOrdering1 should equal (xB)

    swapOrdering1.tipX(0) should be ("0")

    swapOrdering1.tipY(0) should be ("0")
    swapOrdering1.tipY(1) should be ("1")
    swapOrdering1.tipY(2) should be ("2")
    swapOrdering1.tipY(3) should be ("3")
    swapOrdering1.tipY(4) should be ("4")
    swapOrdering1.tipY(5) should be ("5")
    swapOrdering1.tipY(6) should be ("6")

    swapOrdering1.getError should equal (xB.computeError(convolutionSettings) +- delta)

  }

  "An asymetric swap whith a.size < b.size on a big array" should "give the correct matrix and convolution" in {
    val swapOrdering1 = myBigTable.copy
    val reverseSwap = swapOrdering1.swap((1 until 2),(2 to 4))

    swapOrdering1.isFullMatrix should be (true)

    val xA =       DenseMatrix(
        (0.0, 1.0, 1.0, 1.0),
        (1.0, 1.0, 1.0, 0.0),
        (0.0, 1.0, 1.0, 1.0),
        (1.0, 0.0, 1.0, 1.0),
        (1.0, 0.0, 0.0, 1.0),
        (1.0, 1.0, 0.0, 1.0),
        (0.0, 1.0, 1.0, 0.0)
     )

    swapOrdering1 should equal (xA)

    swapOrdering1.tipX(0) should be ("0")

    swapOrdering1.tipY(0) should be ("0")
    swapOrdering1.tipY(1) should be ("2")
    swapOrdering1.tipY(2) should be ("3")
    swapOrdering1.tipY(3) should be ("4")
    swapOrdering1.tipY(4) should be ("1")
    swapOrdering1.tipY(5) should be ("5")
    swapOrdering1.tipY(6) should be ("6")

    swapOrdering1.getError should equal (xA.computeError(convolutionSettings) +- delta)

    /**
    *   Reverse the move
    */
    reverseSwap.reverseTheMove

    val xB = DenseMatrix(
        (0.0, 1.0, 1.0, 1.0),
        (1.0, 0.0, 0.0, 1.0),
        (1.0, 1.0, 1.0, 0.0),
        (0.0, 1.0, 1.0, 1.0),
        (1.0, 0.0, 1.0, 1.0),
        (1.0, 1.0, 0.0, 1.0),
        (0.0, 1.0, 1.0, 0.0)
      )

    // The matrix should be back to the original
    swapOrdering1 should equal (xB)

    swapOrdering1.tipX(0) should be ("0")

    swapOrdering1.tipY(0) should be ("0")
    swapOrdering1.tipY(1) should be ("1")
    swapOrdering1.tipY(2) should be ("2")
    swapOrdering1.tipY(3) should be ("3")
    swapOrdering1.tipY(4) should be ("4")
    swapOrdering1.tipY(5) should be ("5")
    swapOrdering1.tipY(6) should be ("6")

    swapOrdering1.getError should equal (xB.computeError(convolutionSettings) +- delta)

  }



  "An consecutive swap whith a.size == b.size on a big array" should "give the correct matrix and convolution" in {
	val swapOrdering1 = myBigTable.copy
    val reverseSwap = swapOrdering1.swap((0 until 2),(2 until 4))

    swapOrdering1.isFullMatrix should be (true)

    val xA =       DenseMatrix(
        (1.0, 1.0, 1.0, 0.0),
        (0.0, 1.0, 1.0, 1.0),
        (0.0, 1.0, 1.0, 1.0),
        (1.0, 0.0, 0.0, 1.0),
        (1.0, 0.0, 1.0, 1.0),
        (1.0, 1.0, 0.0, 1.0),
        (0.0, 1.0, 1.0, 0.0)
     )

    swapOrdering1 should equal (xA)

    swapOrdering1.tipX(0) should be ("0")

    swapOrdering1.tipY(0) should be ("2")
    swapOrdering1.tipY(1) should be ("3")
    swapOrdering1.tipY(2) should be ("0")
    swapOrdering1.tipY(3) should be ("1")
    swapOrdering1.tipY(4) should be ("4")
    swapOrdering1.tipY(5) should be ("5")
    swapOrdering1.tipY(6) should be ("6")

    swapOrdering1.getError should equal (xA.computeError(convolutionSettings) +- delta)

    /**
    *   Reverse the move
    */
    reverseSwap.reverseTheMove

    swapOrdering1.isFullMatrix should be (true)

    val xB = DenseMatrix(
        (0.0, 1.0, 1.0, 1.0),
        (1.0, 0.0, 0.0, 1.0),
        (1.0, 1.0, 1.0, 0.0),
        (0.0, 1.0, 1.0, 1.0),
        (1.0, 0.0, 1.0, 1.0),
        (1.0, 1.0, 0.0, 1.0),
        (0.0, 1.0, 1.0, 0.0)
      )

    // The matrix should be back to the original
    swapOrdering1 should equal (xB)

    swapOrdering1.tipX(0) should be ("0")

    swapOrdering1.tipY(0) should be ("0")
    swapOrdering1.tipY(1) should be ("1")
    swapOrdering1.tipY(2) should be ("2")
    swapOrdering1.tipY(3) should be ("3")
    swapOrdering1.tipY(4) should be ("4")
    swapOrdering1.tipY(5) should be ("5")
    swapOrdering1.tipY(6) should be ("6")

    swapOrdering1.getError should equal (xB.computeError(convolutionSettings) +- delta)

  }

  "Upside down" should "give the correct matrix" in {
    ordering.copy.upsideDown should equal (
      DenseMatrix(
        (1.0, 1.0, 1.0, 0.0),
        (1.0, 0.0, 0.0, 1.0),
        (0.0, 1.0, 1.0, 1.0)
      )
    )
  }


  val nestedMatrix = BinarySparseMatrix(DenseMatrix(
      (1.0, 0.0, 0.0, 0.0, 0.0, 0.0),
      (1.0, 1.0, 0.0, 0.0, 0.0, 0.0),
      (1.0, 1.0, 1.0, 0.0, 0.0, 0.0),
      (1.0, 1.0, 1.0, 1.0, 0.0, 0.0),
      (1.0, 1.0, 1.0, 1.0, 1.0, 1.0))).upsideDown

  "Shuffle" should "give a correct shuffled matrix and give a nested matrix" in {

    val nestedMatrixShuffled = nestedMatrix.copy.shuffle.t.shuffle.t

    sum(nestedMatrixShuffled.getMatrix) should be (16.0)
    val nestedOrdering = new NestedOrdering()
    val orderedMatrix = nestedMatrixShuffled.copy
    nestedOrdering.changeOrdering(orderedMatrix)

    nestedMatrix should equal (orderedMatrix)
  }

}
