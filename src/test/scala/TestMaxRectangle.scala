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

class TestMaxRectangle extends FlatSpec with Matchers with MatrixHelpers{

  val test1 = BinarySparseMatrix(DenseMatrix(
    (0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0),
    (0.0, 0.0, 0.0, 0.0, 1.0, 1.0, 1.0, 0.0),
    (0.0, 0.0, 0.0, 0.0, 1.0, 1.0, 1.0, 0.0),
    (1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 0.0),
    (1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 0.0, 0.0),
    (0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0)
  ))

  val test2 = BinarySparseMatrix(DenseMatrix(
		(0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0),
		(0.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 0.0, 0.0),
		(0.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0),
		(0.0, 0.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 0.0),
		(0.0, 0.0, 0.0, 1.0, 1.0, 1.0, 1.0, 1.0, 0.0, 0.0, 0.0, 0.0),
		(0.0, 0.0, 0.0, 0.0, 1.0, 1.0, 1.0, 1.0, 0.0, 0.0, 0.0, 0.0),
		(0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 1.0, 1.0, 0.0, 0.0, 0.0, 0.0),
		(0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0)
	))

  val test3 = BinarySparseMatrix(DenseMatrix(
		(0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0),
		(0.0, 0.0, 0.0, 0.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 0.0, 0.0),
		(0.0, 0.0, 0.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 0.0, 0.0),
		(0.0, 0.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 0.0, 0.0),
		(0.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 0.0, 0.0),
		(0.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 0.0, 0.0),
		(0.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 0.0, 0.0, 0.0, 0.0),
		(0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0)
	))

  val test4 = BinarySparseMatrix(DenseMatrix(
		(0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0),
		(0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 1.0, 1.0, 0.0, 0.0, 0.0),
		(0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 1.0, 1.0, 1.0, 0.0, 0.0),
		(0.0, 0.0, 0.0, 0.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 0.0, 0.0),
		(0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 1.0, 1.0, 1.0, 1.0, 0.0, 0.0),
		(0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 1.0, 1.0, 1.0, 0.0, 0.0),
		(0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 1.0, 1.0, 0.0, 0.0, 0.0),
		(0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 1.0, 1.0, 0.0, 0.0, 0.0)
	))

  val test5 = BinarySparseMatrix(DenseMatrix(
		(0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0),
		(0.0, 0.0, 0.0, 0.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 0.0),
		(0.0, 0.0, 0.0, 0.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 0.0),
		(0.0, 0.0, 0.0, 0.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 0.0),
		(0.0, 0.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 0.0, 0.0, 0.0),
		(0.0, 0.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 0.0, 0.0, 0.0),
		(0.0, 0.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 0.0, 0.0, 0.0, 0.0),
		(0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0)
	))

  val test6 = BinarySparseMatrix(DenseMatrix(
		(0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0),
		(0.0, 0.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 0.0, 0.0, 0.0),
		(0.0, 0.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 0.0, 0.0, 0.0),
		(0.0, 0.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 0.0, 0.0),
		(0.0, 0.0, 0.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 0.0, 0.0),
		(0.0, 0.0, 0.0, 0.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 0.0, 0.0),
		(0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 1.0, 1.0, 1.0, 1.0, 0.0, 0.0),
		(0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 1.0, 1.0, 0.0, 0.0, 0.0)
	))

  val test7 = BinarySparseMatrix(DenseMatrix(
		(0.0, 0.0, 0.0, 0.0, 0.0, 0.0),
		(0.0, 0.0, 0.0, 0.0, 0.0, 0.0),
		(0.0, 0.0, 0.0, 1.0, 0.0, 0.0),
		(0.0, 1.0, 0.0, 1.0, 1.0, 0.0),
		(0.0, 1.0, 1.0, 1.0, 1.0, 0.0),
		(0.0, 1.0, 1.0, 1.0, 1.0, 0.0),
		(0.0, 1.0, 1.0, 1.0, 0.0, 0.0),
		(0.0, 0.0, 0.0, 0.0, 0.0, 0.0)
	))

  val testDensity1 = BinarySparseMatrix(DenseMatrix(
    (0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0),
    (0.0, 1.0, 1.0, 0.0, 1.0, 0.0, 0.0, 0.0),
    (0.0, 1.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0),
    (0.0, 1.0, 1.0, 1.0, 1.0, 0.0, 0.0, 0.0),
    (0.0, 1.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0),
    (0.0, 1.0, 1.0, 1.0, 1.0, 0.0, 0.0, 0.0)
  ))

  "Without error" should "find largest rectangle" in {
    // val builder1 = new MaxRectangleBuilderThomas(test1, Coord(3,5), (r:Rectangle)=>r.area, 0)
    // builder1.findMaxRectangle should equal (Rectangle(Coord(3,0), Coord(4,5)))
    //
    // val builder2 = new MaxRectangleBuilderThomas(test2, Coord(2,6), (r:Rectangle)=>r.area, 0)
    // builder2.findMaxRectangle should equal (Rectangle(Coord(1,2), Coord(3,9)))
    //
    // val builder3 = new MaxRectangleBuilderThomas(test3, Coord(3,5), (r:Rectangle)=>r.area, 0)
    // builder3.findMaxRectangle should equal (Rectangle(Coord(1,4), Coord(5,9)))
    //
    // val builder4 = new MaxRectangleBuilderThomas(test4, Coord(4,6), (r:Rectangle)=>r.area, 0)
    // builder4.findMaxRectangle should equal (Rectangle(Coord(1,6), Coord(7,8)))
    //
    // val builder5 = new MaxRectangleBuilderThomas(test5, Coord(4,5), (r:Rectangle)=>r.area, 0)
    // builder5.findMaxRectangle should equal (Rectangle(Coord(1,4), Coord(5,8)))
    //
    // val builder6 = new MaxRectangleBuilderThomas(test6, Coord(3,6), (r:Rectangle)=>r.area, 0)
    // builder6.findMaxRectangle should equal (Rectangle(Coord(1,4), Coord(5,8)))
    //
    // val builder7 = new MaxRectangleBuilderThomas(test7, Coord(4,3), (r:Rectangle)=>r.area, 0)
    // builder7.findMaxRectangle should equal (Rectangle(Coord(4,1), Coord(6,3)))

  }

  "With errors" should "find largest rectangle" in {

//    val builderErrors1 = new MaxRectangleBuilderThomasCorrect(testDensity1, Coord(2,3), (r:Rectangle)=>r.area, 0)
//    builderErrors1.findMaxRectangle should equal (Rectangle(Coord(1,1), Coord(5,4)))

  }

}
