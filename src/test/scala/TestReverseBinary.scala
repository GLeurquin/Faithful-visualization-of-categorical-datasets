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
import breeze.signal.OptPadding

class TestReverseBinary extends FlatSpec with Matchers with MatrixHelpers{


 val columnMat = DenseMatrix(
    (0.0, 0.0, 0.0),
    (0.0, 0.0, 1.0),
    (0.0, 1.0, 0.0),
    (0.0, 1.0, 1.0),
    (1.0, 0.0, 0.0),
    (1.0, 0.0, 1.0),
    (1.0, 1.0, 0.0)
  )

  val columnMat2 = DenseMatrix(
     (0.0, 0.0, 0.0, 0.0),
     (0.0, 0.0, 0.0, 1.0),
     (0.0, 0.0, 1.0, 1.0),
     (0.0, 1.0, 0.0, 0.0),
     (0.0, 1.0, 0.0, 1.0),
     (0.0, 1.0, 1.0, 0.0),
     (0.0, 1.0, 1.0, 1.0),
     (1.0, 0.0, 0.0, 0.0),
     (1.0, 0.0, 0.0, 1.0),
     (1.0, 0.0, 1.0, 0.0),
     (1.0, 0.0, 1.0, 1.0),
     (1.0, 1.0, 0.0, 0.0),
     (1.0, 1.0, 0.0, 1.0),
     (1.0, 1.0, 1.0, 0.0)
   )

val delta = 1e-2
 val convolutionSettings = new ConvolutionSettings(Kernels.rectangleKernel, padding = Padding.ZerosAndOnes)
 val orderingBase:MatrixMoves = BinarySparseMatrix(columnMat, convolutionSettings = convolutionSettings.copy)

 "Reverse" should "work" in {
    val ordering = orderingBase.copy
    val reverseRelocate = ordering.reverse(1,3)
    val swapped = DenseMatrix(
        (0.0, 0.0, 0.0),
        (0.0, 1.0, 1.0),
        (0.0, 1.0, 0.0),
        (0.0, 0.0, 1.0),
        (1.0, 0.0, 0.0),
        (1.0, 0.0, 1.0),
        (1.0, 1.0, 0.0)
      )
    ordering should equal (swapped)

    ordering.tipX(0) should be ("0")

    ordering.tipY(0) should be ("0")
    ordering.tipY(1) should be ("3")
    ordering.tipY(2) should be ("2")
    ordering.tipY(3) should be ("1")
    ordering.tipY(4) should be ("4")
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


  it should "work again" in {
    val ordering = orderingBase.copy
    val reverseRelocate = ordering.reverse(0,6)
    val swapped = DenseMatrix(
        (1.0, 1.0, 0.0),
        (1.0, 0.0, 1.0),
        (1.0, 0.0, 0.0),
        (0.0, 1.0, 1.0),
        (0.0, 1.0, 0.0),
        (0.0, 0.0, 1.0),
        (0.0, 0.0, 0.0)
      )
    ordering should equal (swapped)


    ordering.tipX(0) should be ("0")

    ordering.tipY(0) should be ("6")
    ordering.tipY(1) should be ("5")
    ordering.tipY(2) should be ("4")
    ordering.tipY(3) should be ("3")
    ordering.tipY(4) should be ("2")
    ordering.tipY(5) should be ("1")
    ordering.tipY(6) should be ("0")

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

  it should "work again with larger kernel" in {
    val convolutionSettingsBigger = new ConvolutionSettings(Kernels.gaussianBlurExp(5), padding = Padding.ZerosAndOnes)
    val ordering = BinarySparseMatrix(columnMat2, convolutionSettings = convolutionSettingsBigger)
    val reverseRelocate = ordering.reverse(5,7)
    val swapped = DenseMatrix(
      (0.0, 0.0, 0.0, 0.0),
      (0.0, 0.0, 0.0, 1.0),
      (0.0, 0.0, 1.0, 1.0),
      (0.0, 1.0, 0.0, 0.0),
      (0.0, 1.0, 0.0, 1.0),
      (1.0, 0.0, 0.0, 0.0),
      (0.0, 1.0, 1.0, 1.0),
      (0.0, 1.0, 1.0, 0.0),
      (1.0, 0.0, 0.0, 1.0),
      (1.0, 0.0, 1.0, 0.0),
      (1.0, 0.0, 1.0, 1.0),
      (1.0, 1.0, 0.0, 0.0),
      (1.0, 1.0, 0.0, 1.0),
      (1.0, 1.0, 1.0, 0.0)
      )
    ordering should equal (swapped)

    val theConvolutionOrig = swapped.convolute(convolutionSettingsBigger)
    theConvolutionOrig should equal (ordering.getConvolution)

    ordering.getError should equal (swapped.computeError(convolutionSettingsBigger) +- delta)


    /**
    *   Reverse the move
    */
    reverseRelocate.reverseTheMove
    ordering should equal (columnMat2)

    val theConvolution2 = columnMat2.convolute(convolutionSettingsBigger)
    theConvolution2 should equal (ordering.getConvolution)

    ordering.getError should equal (columnMat2.computeError(convolutionSettingsBigger) +- delta)

  }

  it should "work again 2" in {
    val convolutionSettings = new ConvolutionSettings(Kernels.gaussianBlurExp(3), padding = Padding.ZerosAndOnes)
    val ordering = BinarySparseMatrix(columnMat, convolutionSettings = convolutionSettings)
    val reverseRelocate = ordering.reverse(0,3)
    val swapped = DenseMatrix(
      (0.0, 1.0, 1.0),
      (0.0, 1.0, 0.0),
      (0.0, 0.0, 1.0),
      (0.0, 0.0, 0.0),
      (1.0, 0.0, 0.0),
      (1.0, 0.0, 1.0),
      (1.0, 1.0, 0.0)
      )

    ordering should equal (swapped)

    val theConvolutionOrig = swapped.convolute(convolutionSettings)
    theConvolutionOrig should equal (ordering.getConvolution)

    ordering.getError should equal (swapped.computeError(convolutionSettings) +- delta)


    /**
    *   Reverse the move
    */
    reverseRelocate.reverseTheMove
    ordering should equal (columnMat)

    val theConvolution2 = columnMat.convolute(convolutionSettings)
    theConvolution2 should equal (ordering.getConvolution)

    ordering.getError should equal (columnMat.computeError(convolutionSettings) +- delta)

  }


}
