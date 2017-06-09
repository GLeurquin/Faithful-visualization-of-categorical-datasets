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

class TestInterval extends FlatSpec with Matchers {

	val ai = Interval(3, 5)
	val bi = Interval(-1,0)
	val ci = Interval(-1,-1)

	val ei = Interval(3,6)
	val fi = Interval(2,7)
	val gi = Interval(0,0)


	s"$ai" should s"be included in $ei" in {
		ai isIncludedIn ei should be (true) 
	}

	it should s"be included in $fi" in {
		ai isIncludedIn fi should be (true) 
	}

	it should s"not be empty" in {
		ai.isEmpty should be (false) 
	}

	it should s"be before $ei" in {
		ai should be < ei
	}

	s"$bi" should s"be empty" in {
		bi.isEmpty should be (true) 
	}

	s"$ci" should s"be empty" in {
		ci.isEmpty should be (true) 
	}

	s"$gi size" should s"should be 0" in {
		gi.size should be (0) 
	}

	s"$fi size" should s"should be 5" in {
		fi.size should be (5) 
	}

	

}