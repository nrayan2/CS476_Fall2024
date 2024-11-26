object FuzzyLogic {
  sealed trait Expr
  case class Value(v: Double) extends Expr // constant values
  case class Variable(name: String) extends Expr // unresolved variables
  case class Multiply(lhs: Expr, rhs: Expr) extends Expr
  case class Add(lhs: Expr, rhs: Expr) extends Expr
  case class IFTRUE(cond: Expr, thenBranch: Expr, elseBranch: Expr) extends Expr
  case class FuzzyElement(name: String, membership: Double)



  def partialEvaluate(expr: Expr): Expr = expr match {
  case Multiply(Value(0), _) | Multiply(_, Value(0)) => Value(0)
  case Add(lhs, rhs) if lhs == rhs => Multiply(Value(2), partialEvaluate(lhs))
  case Multiply(Value(a), Value(b)) => Value(a * b)
  case Multiply(Value(1), rhs) => partialEvaluate(rhs)
  case Multiply(lhs, Value(1)) => partialEvaluate(lhs)
  case Multiply(lhs, rhs) => Multiply(partialEvaluate(lhs), partialEvaluate(rhs))
  case Add(Value(a), Value(b)) => Value(a + b)
  case Add(Value(0), rhs) => partialEvaluate(rhs)
  case Add(lhs, Value(0)) => partialEvaluate(lhs)
  case Add(lhs, rhs) => Add(partialEvaluate(lhs), partialEvaluate(rhs))
  case IFTRUE(cond, thenBranch, elseBranch) =>
    IFTRUE(partialEvaluate(cond), partialEvaluate(thenBranch), partialEvaluate(elseBranch))
  case other => other
}


  def evaluate(expr: Expr, env: Map[String, Double]): Double = expr match {
    case Value(v) => v
    case Variable(name) => env.getOrElse(name, throw new Exception(s"Variable $name not found"))
    case Multiply(lhs, rhs) => evaluate(lhs, env) * evaluate(rhs, env)
    case Add(lhs, rhs) => evaluate(lhs, env) + evaluate(rhs, env)
    case IFTRUE(cond, thenBranch, elseBranch) =>
      if (evaluate(cond, env) != 0) evaluate(thenBranch, env) else evaluate(elseBranch, env)
  }

  // fuzzy union between 2 fuzzy sets
  // for each pair of elements from A and B, it takes the maximum membership value
  // This is representative of the OR operation, getting the highest degree of truth

  def fuzzyUnion(setA: List[FuzzyElement], setB: List[FuzzyElement]): List[FuzzyElement] = {
    setA.zip(setB).map { case (a, b) => FuzzyElement(a.name, math.max(a.membership, b.membership)) }
  }


  // fuzzy intersection between 2 fuzzy sets
  // takes minimum value for each pair of elements
  // Represents the AND operation
  def fuzzyIntersection(setA: List[FuzzyElement], setB: List[FuzzyElement]): List[FuzzyElement] = {
    setA.zip(setB).map { case (a, b) => FuzzyElement(a.name, math.min(a.membership, b.membership)) }
  }

  // fuzzy complement on a fuzzy set
  // Returns the complement of each element be subtracting the membership from 1
  def fuzzyComplement(set: List[FuzzyElement]): List[FuzzyElement] = {
    set.map(e => FuzzyElement(e.name, 1 - e.membership))
  }

  // fuzzy addition between 2 fuzzy sets
  // adds membership values for each pair of elements
  def fuzzyAddition(setA: List[FuzzyElement], setB: List[FuzzyElement]): List[FuzzyElement] = {
    setA.zip(setB).map { case (a, b) => FuzzyElement(a.name, math.min(1, a.membership + b.membership)) }
  }

  // fuzzy multiplication between 2 fuzzy sets
  // multiplies the membership values of corresponding elements 
  def fuzzyMultiplication(setA: List[FuzzyElement], setB: List[FuzzyElement]): List[FuzzyElement] = {
    setA.zip(setB).map { case (a, b) => FuzzyElement(a.name, a.membership * b.membership) }
  }

  // perform fuzzy XOR
  // calculates the absolute diff between memberships
  def fuzzyXOR(setA: List[FuzzyElement], setB: List[FuzzyElement]): List[FuzzyElement] = {
    setA.zip(setB).map { case (a, b) => FuzzyElement(a.name, math.abs(a.membership - b.membership)) }
  }

  // alpha cut on a fuzzy set
  // filters the elements of the set only keeping the ones with a membership >= alpha
  def alphaCut(set: List[FuzzyElement], alpha: Double): List[FuzzyElement] = {
    set.filter(_.membership >= alpha)
  }
}
