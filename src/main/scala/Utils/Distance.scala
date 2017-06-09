package thesis.utils

import scala.collection.mutable.HashMap
import breeze.linalg._
import breeze.numerics._

/** Object defining sereval measures of distance between two vectors
*/
object Distance {

	/** Number of elements that are not the same
	*/
	def sum_diff(mat: DenseMatrix[Double], i: Int, j: Int) = {
		var count = 0.0

		for(c <- 0 until mat.cols){
			if(mat(i,c) != mat(j,c)) count += 1.0
		}
		count
	}

	/** Number of different pairs of colors.
	* 	The more pairs there are between the vectors, the more different the vectors will be
	*/
	def sum_diffCat(mat: DenseMatrix[Double], i: Int, j: Int) = {

		var set = Set[(Double, Double)]()

		(0 until mat.cols).foreach{ c =>
			val v = (mat(i,c), mat(j,c))
			set = set + v
		}

		set.size.toDouble
	}

	/** Number of elements minus number of times the most frequent pair of colors appears
	*	If it appears very often, this value will be small
	*/
	def sum_diffCat2(mat: DenseMatrix[Double], i: Int, j: Int) = {

		val map = HashMap[(Int,Int), Int]()

		(0 until mat.cols).foreach{ c =>
    		map += (mat(i,c).toInt,mat(j,c).toInt) -> (map.getOrElse((mat(i,c).toInt,mat(j,c).toInt), 0) + 1)
    	}

    	(mat.cols - map.valuesIterator.max).toDouble
	}

	/** Square of the number of elements minus the
	* 	sum of the squares of the frequences of each different pairs of colors
	* 	The fewer pairs and the higher their frequences, the smaller this value
	*/
	def sum_diffCat4(mat: DenseMatrix[Double], i: Int, j: Int) = {

		val map = HashMap[(Int,Int), Int]()

		(0 until mat.cols).foreach{ c =>
    		map += (mat(i,c).toInt,mat(j,c).toInt) -> (map.getOrElse((mat(i,c).toInt,mat(j,c).toInt), 0) + 1)
    	}

		var sum = 0.0
		map foreach {case (key, value) => sum += value*value}
		mat.cols * mat.cols - sum
	}

	/** idem as sum_diffCat2 but add a bonus if several identical pairs are adjacent
	*	@see sum_diffCat2
	*/
	def sum_diffCat3(mat: DenseMatrix[Double], i: Int, j: Int) = {
			var error = 0.0
			val map = HashMap[(Int,Int), Int]()

		 (0 until mat.cols).foreach{ c =>
			 map += (mat(i,c).toInt,mat(j,c).toInt) -> (map.getOrElse((mat(i,c).toInt,mat(j,c).toInt), 0) + 1)
			}

		error += (mat.cols - map.valuesIterator.max)

		var succ = 0

	    (1 until mat.cols).foreach{ c =>
	      if(mat(i,c)==mat(i-1,c)){
	        succ += 1
	        error += 1.0/math.pow(1.3,succ)
	      }
	      else{
	        succ=0
	        error+=1
	      }
		}

		error
	}
}
