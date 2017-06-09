package thesis.utils

/**
*	Class representing an interval (first and last inclusive)
*/
case class Interval(val first:Int, val last:Int) extends Ordered[Interval]{
	require(first <= last, s"first($first) must be <= than last ($last)")
	val isEmpty = first < 0 || last < 0

	val size = last-first

	/** Sort by the beginning of the interval first, then by its end to deal with ties
	*/
	def compare(that: Interval) = {
		val x = first - that.first
		if(x==0) last - that.last
		else x
	}

	/**
	*	@return true if this interval is included in the other
	* 	@example (0 1 1 1 0 0) is included in (0 1 1 1 1 1)
	*/
	def isIncludedIn(other:Interval):Boolean = {
		if(this.isEmpty) return true
		if(other.isEmpty) return false
		(first >= other.first) && (last <= other.last)
	}

	/**
	*	@return true if this interval is strictly included in the other
	* 	@example (0 0 1 1 0 0) is strictly included in (0 1 1 1 1 1)
	*/
	def isStrictlyIncludedIn(other:Interval):Boolean = {
		if(this.isEmpty) return true
		if(other.isEmpty) return false
		(first > other.first) && (last < other.last)
	}

	/**
	*	@note pre: this interval included in the other
	*	@return the difference between this interval and the other
	*/
	def numberOfTransformations(other:Interval):Int = {
		require(isIncludedIn(other), s"Interval $this must be included in interval $other")

		val theDiff = first - other.first + other.last - last
		assume(theDiff >= 0, "the difference must be greater than 0")
		theDiff
	}

	/**
	*	@note pre: other interval included in this interval
	*	Returns the left and right intervals representing this without other (this\other)
	*	@example
	*	a : 0 0 1 1 1 1 1 0 0 0
	* 	b : 0 0 0 0 1 1 0 0 0 0
	* 	->:     2 3     6
	*		left(2,3) 	right(6,6)
	*/
	def diff(other:Interval):(Interval, Interval) = {
		require(other.isIncludedIn(this), "vector b must be included in a")

		val left = Interval(first, (other.first-1).max(first))
		val right = Interval((other.last+1).min(last), last)
		(left, right)
	}
}
