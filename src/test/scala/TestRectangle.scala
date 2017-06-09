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


class TestRectangle extends FlatSpec with Matchers with MatrixHelpers{

	val r1 = Rectangle(Coord(0,0), Coord(0, 5))
	val r2 = Rectangle(Coord(0,0), Coord(0, 0))
	val r3 = Rectangle(Coord(0,0), Coord(5,5))
	val r4 = Rectangle(Coord(2,2), Coord(2,3))
	val r5 = Rectangle(Coord(0,6), Coord(0,10))


	s"$r1" should s"overlap rows with $r2" in {
		assert(r1.overlapsRows(r2))
		assert(r2.overlapsRows(r1))
	}

	it should s"overlap columns with $r2" in {
		assert(r1.overlapsCols(r2))
		assert(r2.overlapsCols(r1))
	}

	it should s"have an area of 6" in {
		r1.area should be (6)
	}

	it should s"have a height of 1" in {
		r1.height should be (1)
	}

	it should s"have a width of 6" in {
		r1.width should be (6)
	}

	it should s"touch columns with $r5" in {
		assert(r1.touchCols(r5))
		assert(r5.touchCols(r1))
	}

	s"$r3" should s"overlap columns with $r4" in {
		assert(r3.overlapsCols(r4))
		assert(r4.overlapsCols(r3))
	}

	it should s"overlap rows with $r4" in {
		assert(r3.overlapsRows(r4))
		assert(r4.overlapsRows(r3))
	}

	it should s"have an area of 36" in {
		r3.area should be (36)
	}

	s"The area of $r2" should s"be 1" in {
		r2.area should be (1)
	}





}
