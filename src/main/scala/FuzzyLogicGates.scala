import GateOperations._
import scala.collection.mutable

// case class for fuzzy gates
case class FuzzyGate(name: String, operation: List[Double] => Double)

// class representation for HW2
case class ClassDef(
  name: String,
  variables: mutable.Map[String, Any] = mutable.Map(),
  methods: mutable.Map[String, MethodDef] = mutable.Map(),
  parent: Option[ClassDef] = None
) {
  
  def addVariable(varName: String, value: Any): Unit = {variables(varName) = value}
  def addMethod(MethodName: String, method: MethodDef): Unit = {methods(MethodName) = method}
  def getVariable(varName: String): Option[Any] = {variables.get(varName).orElse(parent.flatMap(_.getVariable(varName)))}
  def InvokeMethod(methodName: String, params: Map[String, Any]): Any = {
    methods.get(methodName)
      .orElse(parent.flatMap(_.methods.get(methodName)))
      .map(_.execute(params))
      .getOrElse(throw new NoSuchElementException("Method can't be found"))
  }
}

case class MethodDef(parameters: List[String], body: Map[String, Any] => Any) {def execute(params: Map[String, Any]): Any = body(params)}

object FuzzyLogicGates {
  // map to store gates and operations
  private val gates = mutable.Map[String, FuzzyGate]()
  private val classRegistry: mutable.Map[String, FuzzyGate] = mutable.Map()

  // helper to assign gate with its operation
  // allows user to define custom logic gates with behavior
  def assignGate(name: String, operation: List[Double] => Double): Unit = {
    GateOperations.assignGate(name, operation)
  }

  // helper to evaluate a gate given input values
  // if gate found, applies the gate's operation, and otherwise, return none
  def evaluateGate(gateName: String, inputs: List[Double]): Option[Double] = {
    GateOperations.evaluateGate(gateName, inputs)
  }

  def defineClass(className: String, methodName: String, method: List[Double] =>Double): Unit =
    {
      classRegistry.get(className).foreach
    } 

}