package thesis.utils

/** Array for which up to size elements are added before starting over at the beginning and overwriting previous elements.
*	Provides a gradient function to compute the difference between the earliest and oldest element of the array
*/
class CyclicArray(size:Int){
	require(size > 0)
	private var max_seen = 10e-10 // Maximum of the array

	private var _mean = 0.0
	def mean = _mean/max_seen // normalized moving average

	private var currentIdx = 0

	private val array = new Array[Double](size)

	private var numberOfElements = 0

	/** Adds the value at the end of this cyclic array
	*	
	*	@param value The value to add to this array
	*/
	def +=(value:Double) {
		assert(value >= 0.0)
		val oldValue = array(currentIdx)
		_mean += (value - oldValue)/size


		array(currentIdx) = value // override the old value

		if(value > max_seen) max_seen = value
		else if(max_seen == oldValue) max_seen = array.max

		currentIdx += 1
		if(currentIdx >= size) currentIdx = 0

		numberOfElements += 1
		numberOfElements.max(size)
	}

	/**
	*	@return the element last added to the array
	*/ 
	def earliest = {
		if(currentIdx-1 < 0) array(numberOfElements-1)
		else array(currentIdx-1)
	}

	/**
	*	@return the oldest element added to the array
	*/
	def oldest = {
		if(numberOfElements >= size) array(currentIdx)
		else array(0)
	}

	/**
	*	@return the gradient between the earliest and oldest element of this array
	*	It should be < 0 except at the begining, since elements are initialized at 0
	*/
	def gradient = (earliest - oldest)/numberOfElements.toDouble 

}
