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

class TestRelocateBinary extends FlatSpec with Matchers with MatrixHelpers{


 val columnMat = DenseMatrix(
    (0.0, 0.0, 0.0),
    (0.0, 0.0, 1.0),
    (0.0, 1.0, 0.0),
    (0.0, 1.0, 1.0),
    (1.0, 0.0, 0.0),
    (1.0, 0.0, 1.0),
    (1.0, 1.0, 0.0)
  )

 val convolutionSettings = new ConvolutionSettings(Kernels.rectangleKernel, padding=Padding.ZerosAndOnes)
 val orderingBase:MatrixMoves = BinarySparseMatrix(columnMat, convolutionSettings=convolutionSettings.copy)

 "Relocate" should "work backwards" in {
    val ordering = orderingBase.copy
    val reverseRelocate = ordering.relocate(4,2,2)
    val swapped = DenseMatrix(
        (0.0, 1.0, 0.0),
        (0.0, 1.0, 1.0),
        (1.0, 0.0, 0.0),
        (0.0, 0.0, 0.0),
        (0.0, 0.0, 1.0),
        (1.0, 0.0, 1.0),
        (1.0, 1.0, 0.0)
      )
    ordering should equal (swapped)

    ordering.tipX(0) should be ("0")

    ordering.tipY(0) should be ("2")
    ordering.tipY(1) should be ("3")
    ordering.tipY(2) should be ("4")
    ordering.tipY(3) should be ("0")
    ordering.tipY(4) should be ("1")
    ordering.tipY(5) should be ("5")
    ordering.tipY(6) should be ("6")


    val theConvolutionOrig = swapped.convolute(convolutionSettings)
    theConvolutionOrig should equal (ordering.getConvolution)

    ordering.getError should equal (swapped.computeError(convolutionSettings))

    reverseRelocate.reverseTheMove
    ordering should equal (columnMat)

    val theConvolution2 = columnMat.convolute(convolutionSettings)
    theConvolution2 should equal (ordering.getConvolution)

    ordering.getError should equal (columnMat.computeError(convolutionSettings))


    ordering.tipX(0) should be ("0")

    ordering.tipY(0) should be ("0")
    ordering.tipY(1) should be ("1")
    ordering.tipY(2) should be ("2")
    ordering.tipY(3) should be ("3")
    ordering.tipY(4) should be ("4")
    ordering.tipY(5) should be ("5")
    ordering.tipY(6) should be ("6")

 }


  it should "work" in {
    val ordering = orderingBase.copy
    val reverseRelocate = ordering.relocate(1,3,3)
    val swapped = DenseMatrix(
        (0.0, 0.0, 0.0),
        (1.0, 0.0, 0.0),
        (1.0, 0.0, 1.0),
        (1.0, 1.0, 0.0),
        (0.0, 0.0, 1.0),
        (0.0, 1.0, 0.0),
        (0.0, 1.0, 1.0)
      )
    ordering should equal (swapped)


    ordering.tipX(0) should be ("0")

    ordering.tipY(0) should be ("0")
    ordering.tipY(1) should be ("4")
    ordering.tipY(2) should be ("5")
    ordering.tipY(3) should be ("6")
    ordering.tipY(4) should be ("1")
    ordering.tipY(5) should be ("2")
    ordering.tipY(6) should be ("3")

    val theConvolutionOrig = swapped.convolute(convolutionSettings)
    theConvolutionOrig should equal (ordering.getConvolution)

    ordering.getError should equal (swapped.computeError(convolutionSettings))


    /**
    *   Reverse the move
    */
    reverseRelocate.reverseTheMove
    ordering should equal (columnMat)

    val theConvolution2 = columnMat.convolute(convolutionSettings)
    theConvolution2 should equal (ordering.getConvolution)

    ordering.getError should equal (columnMat.computeError(convolutionSettings))


    ordering.tipX(0) should be ("0")

    ordering.tipY(0) should be ("0")
    ordering.tipY(1) should be ("1")
    ordering.tipY(2) should be ("2")
    ordering.tipY(3) should be ("3")
    ordering.tipY(4) should be ("4")
    ordering.tipY(5) should be ("5")
    ordering.tipY(6) should be ("6")
  }


}
