package thesis.orderings
import thesis.matrixTypes._
import thesis.utils._

import breeze.linalg._, eigSym.EigSym
import breeze.stats._

class SpectralOrdering(f: =>(DenseVector[Double], DenseVector[Double])=>Double) extends MatrixOrdering {
	override def toString:String = "Spectral"


	/**
	*	@param m The matrix on which to apply the spectral ordering
	*	@return a column ordering
	*/
	private def spectralOrdering(m:DenseMatrix[Double]):Array[Int] = {

		def pairMap(a:Int, b:Int):Double = f(m(::,a), m(::,b))
		val s = DenseMatrix.tabulate[Double](m.cols, m.cols)(pairMap)

		def myTabulate(a:Int, b:Int):Double = {
			if(a!=b) 0.0
			else sum(s(a,::))
		}
		val d = DenseMatrix.tabulate[Double](s.rows, s.cols)(myTabulate)
		val l = d-s // Laplacian matrix

		// computes the eigenvectors/values of l
		val EigSym(lambda, evs) = eigSym(l)

		val e2 = evs(::,1) // eigenvector associated to the second biggest eigenvalue

		// sort this eigenvector
		argsort(e2).toArray
	}

	/**
	*	@inheritdoc
	*/
	override def changeOrdering(m:MatrixMoves) = {
		if(m.isCat) throw new IllegalArgumentException("Spectral ordering only works for binary data")
		val colOrder = spectralOrdering(m.getMatrix)
		m.t.permute(colOrder)
		val rowOrder = spectralOrdering(m.getMatrix)
		m.t.permute(rowOrder)
		m
	}

}


object SpectralOrdering {

	/**	Values range from 1 (identical columns) to 0 (anticorrelated columns)
	*/
	def corr(a: DenseVector[Double], b: DenseVector[Double]): Double = {
		require (a.length == b.length, s"Vectors a(${a.length}) and b(${b.length}) have to be of the same length" )

		val n = a.length

		val ameanavar = meanAndVariance(a)
		val amean = ameanavar.mean
		val avar = ameanavar.variance
		val bmeanbvar = meanAndVariance(b)
		val bmean = bmeanbvar.mean
		val bvar = bmeanbvar.variance
		val astddev = math.sqrt(avar)
		val bstddev = math.sqrt(bvar)

		1.0 / (n - 1.0) * sum( ((a - amean) / astddev) :* ((b - bmean) / bstddev) )
	}


	/**
	* 	@return the Pearson correlation coefficient between a and b
	* 	@note WARNING: It is not defined if either a or b has only the same value
	*/
	def pearson(a:DenseVector[Double],b:DenseVector[Double]):Double = (1+corr(a,b))/2

	/**
	* 	@return the dot product between a and b
	*/
	def dot(a:DenseVector[Double], b:DenseVector[Double]):Double =  a.dot(b)

	/**
	* 	@return the number of values in common between a and b
	*/
	def perso(a:DenseVector[Double], b:DenseVector[Double]):Double = {
		var count = 0

		for(i <- 0 until a.length){
			if(a(i) == b(i)) count += 1
		}

		count
	}
}
