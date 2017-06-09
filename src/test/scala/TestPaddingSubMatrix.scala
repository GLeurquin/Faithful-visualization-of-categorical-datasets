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

class PaddingSubMatrix extends FlatSpec with Matchers with MatrixHelpers{

  val mat = DenseMatrix(
    //        S   S   S               -3
    (1.0,1.0,0.0,0.0,0.0,1.0,1.0), // -2
    (0.0,0.0,1.0,1.0,1.0,1.0,0.0),// -1
    (1.0,1.0,0.0,0.0,0.0,0.0,1.0), // SUB 0
    (1.0,0.0,0.0,0.0,0.0,1.0,0.0), // SUB 1
    (0.0,1.0,0.0,0.0,0.0,0.0,0.0), // SUB 2
    (0.0,0.0,0.0,0.0,0.0,1.0,1.0), // SUB 3
    (1.0,0.0,1.0,0.0,1.0,0.0,0.0), // 4
    (1.0,0.0,0.0,1.0,0.0,0.0,1.0), // 5
    (0.0,1.0,1.0,0.0,0.0,1.0,1.0) //6
                                  //7
  )
  val convolutionSettings = ConvolutionSettings(Kernels.gaussianBlurSmooth(3,3), ErrorType.Abs, Padding.Zero)

  val matrix = BinarySparseMatrix(mat, convolutionSettings=convolutionSettings)


  "Padding" should "work inside the matrix" in {
      val matrixCopy = matrix.copy
      val rowRange = (2 to 5)
      val colRange = (2 to 4)
      matrixCopy.subMatrix(rowRange, colRange)
      val deltaRow = rowRange.min
      val deltaCol = colRange.min

      for(r <- -1 to matrix.rows; c <- -1 to matrix.cols){
          matrix.getWithPadding(r,c) should equal (matrixCopy.getWithPadding(r - deltaRow,c - deltaCol))
      }

  }

  it should "on the side (row)" in {
      val matrixCopy = matrix.copy
      val rowRange = (0 to 5)
      val colRange = (2 to 4)
      matrixCopy.subMatrix(rowRange, colRange)
      val deltaRow = rowRange.min
      val deltaCol = colRange.min

      for(r <- -1 to matrix.rows; c <- -1 to matrix.cols){
          matrix.getWithPadding(r,c) should equal (matrixCopy.getWithPadding(r - deltaRow,c - deltaCol))
      }

  }

  it should "on the side (col)" in {

      val matrixCopy = matrix.copy
      val rowRange = (2 to 5)
      val colRange = (2 to 6)
      matrixCopy.subMatrix(rowRange, colRange)
      val deltaRow = rowRange.min
      val deltaCol = colRange.min

      for(r <- -1 to matrix.rows; c <- -1 to matrix.cols){
          matrix.getWithPadding(r,c) should equal (matrixCopy.getWithPadding(r - deltaRow,c - deltaCol))
      }

  }

  it should "on the side (row and col)" in {

      val matrixCopy = matrix.copy
      val rowRange = (0 to 5)
      val colRange = (2 to 6)
      matrixCopy.subMatrix(rowRange, colRange)
      val deltaRow = rowRange.min
      val deltaCol = colRange.min

      for(r <- -1 to matrix.rows; c <- -1 to matrix.cols){
          matrix.getWithPadding(r,c) should equal (matrixCopy.getWithPadding(r - deltaRow,c - deltaCol))
      }

  }

  it should "on the whole matrix" in {

      val matrixCopy = matrix.copy
      val rowRange = (0 to 8)
      val colRange = (0 to 6)
      matrixCopy.subMatrix(rowRange, colRange)
      val deltaRow = rowRange.min
      val deltaCol = colRange.min

      for(r <- -1 to matrix.rows; c <- -1 to matrix.cols){
          matrix.getWithPadding(r,c) should equal (matrixCopy.getWithPadding(r - deltaRow,c - deltaCol))
      }

  }

  "Brute Force" should "work" in {
      val matrixCopy = matrix.copy
      val brute = new BruteForceImproved()
      val r = Rectangle(Coord(2,0),Coord(5,mat.cols-1))
      val decisions = matrixCopy.subMatrix(r)

      brute.changeOrdering(matrixCopy)

      val moveReverse = matrixCopy.insertSubmatrix()
  }


}
