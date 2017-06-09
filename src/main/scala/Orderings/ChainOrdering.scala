package thesis.orderings
import thesis.matrixTypes._
import thesis.utils._

/** Chains multiple orderings one after the other
*/
class ChainOrdering(orderings:MatrixOrdering*) extends MatrixOrdering {
     val firstOrdering = if(orderings.isEmpty) new Identity() else orderings.head
     override def toString = orderings.map(_.toString).mkString("-")
     val desc = toString
     override def changeOrdering(m:MatrixMoves) = {
          orderings.foreach{_.changeOrdering(m)}
          m
     }
     lazy val skipFirst = new ChainOrdering(orderings.tail: _*){
          override def toString = desc
     }
}