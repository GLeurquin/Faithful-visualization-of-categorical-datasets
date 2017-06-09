package thesis.utils

/** Error type for the convolution
*/
abstract class ErrorType
object ErrorType {
	case object Abs extends ErrorType
	case object Square extends ErrorType

	val errorTypes = List(Abs, Square)
}

/** Settings for the convolution
*	
*	@param theKernel Odd-sized kernel
*	@param errorType The type of error to compute (abs or square)
*	@param padding The type of padding to apply on the edge of the matrix when convoluting
*/
case class ConvolutionSettings(kernel:Kernel, errorType: ErrorType = ErrorType.Abs,padding:Padding = Padding.ZerosAndOnes){
	override def toString:String = s"${kernel + "-" + errorType + "-" + padding}"
	def copy = ConvolutionSettings(kernel.copy, errorType, padding)

	if(padding == Padding.Boundary) assert(kernel.rows == 3 && kernel.cols==3, "boundary padding only works with a 3x3 kernel")

	def t = ConvolutionSettings(kernel.copy.t, errorType, padding)
}
