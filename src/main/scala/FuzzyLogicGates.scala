import GateOperations._
import scala.collection.mutable

// case class for fuzzy gates
case class FuzzyGate(name: String, operation: List[Double] => Double)

object FuzzyLogicGates {
  // map to store gates and operations
  private val gates = mutable.Map[String, FuzzyGate]()

  // helper to assign gate with its operation
  def assignGate(name: String, operation: List[Double] => Double): Unit = {
    GateOperations.assignGate(name, operation)
  }

  // helper to evaluate a gate given input values
  def evaluateGate(gateName: String, inputs: List[Double]): Option[Double] = {
    GateOperations.evaluateGate(gateName, inputs)
  }
}