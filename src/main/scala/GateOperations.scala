import scala.collection.mutable

object GateOperations {
  // mutable map to store gate assignments
  private val gateRegistry = mutable.Map[String, List[Double] => Double]()
  private val classes = mutable.Map[String, ClassDef]()

  def createClass(name: String, parent: Option[String] = None): ClassDef = {
    val parentClass = parent.flatMap(classes.get)
    val newClass = ClassDef(name, parent = parentClass)
    classes(name) = newClass
    newClass
  }

  // assign a gate to variable name
  def assignGate(name: String, gate: List[Double] => Double): Unit = {
    gateRegistry(name) = gate
  }

  // evaluate gate by name with input values
  def evaluateGate(name: String, inputs: List[Double]): Option[Double] = {
    gateRegistry.get(name).map(gate => gate(inputs))
  }

  // stack to handle scopes
  private val scopes = mutable.Stack[mutable.Map[String, Double]]()

  // enter new scope
  def enterScope(): Unit = {
    scopes.push(mutable.Map[String, Double]())
  }

  // exit current scope
  def exitScope(): Unit = {
    if (scopes.nonEmpty) scopes.pop()
  }

  // assign var within the current scope
  def assignVariable(name: String, value: Double): Unit = {
    if (scopes.nonEmpty) {
      scopes.top(name) = value
    } else {
      // global scope
      val globalScope = mutable.Map[String, Double](name -> value)
      scopes.push(globalScope)
    }
  }

  def getVariable(name: String): Option[Double] = {
    scopes.find(_.contains(name)).flatMap(_.get(name))
  }

  // test for eval gate
  def testGate(name: String, inputs: List[Double], expected: Double): Boolean = {
    evaluateGate(name, inputs).contains(expected)
  }

  def xor(inputs: List[Double]): Double = {
    // computs absolute diff iteratively
    inputs.reduce((a, b) => math.abs(a - b))
  }

  def addVariable(className: String, varName: String, value: Any): Unit = {classes.get(className).foreach(_.addVariable(varName, value))}
  def addMethod(className: String, methodName: String, method: MethodDef): Unit = {classes.get(className).foreach(_.addMethod(methodName, method))}
  def createInstance(className: String): ClassDef = {classes.getOrElse(className, throw new NoSuchElementException("class is not found"))}
  def InvokeMethod(instance: ClassDef, methodName: String, params: Map[String, Any]): Any = {instance.InvokeMethod(methodName, params)}
}