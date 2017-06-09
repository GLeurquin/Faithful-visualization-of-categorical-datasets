package thesis.utils

abstract class Padding

object Padding{
  case object BoundaryAsymmetric extends Padding
  case object Cyclical extends Padding
  case object Boundary extends Padding
  case class  ValueOpt[T](value: T) extends Padding
  case object Zero extends Padding
  case object ZerosAndOnes extends Padding
}
