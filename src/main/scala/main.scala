import FuzzyLogic._
import FuzzyLogicGates.{evaluateGate => evalGateFromFuzzyLogicGates}
import GateOperations.{assignGate, evaluateGate, enterScope, exitScope, assignVariable, getVariable, testGate, gateRegistry}

object Main {
  def main(args: Array[String]): Unit = {
    // core fuzzy logic operations
    val setA = List(FuzzyElement("x1", 0.4), FuzzyElement("x2", 0.7))
    val setB = List(FuzzyElement("x1", 0.5), FuzzyElement("x2", 0.6))

    // combines membership values by taking the max for each element
    println("Fuzzy Union: " + fuzzyUnion(setA, setB))
    // combines membership values by taking the min for each element
    println("Fuzzy Intersection: " + fuzzyIntersection(setA, setB))
    // calculates the complement of each membership value (1 - membership)
    println("Fuzzy Complement: " + fuzzyComplement(setA))
    // sums the membership values of two sets but ensures values don't exceed 1
    println("Fuzzy Addition: " + fuzzyAddition(setA, setB))
    // multiplies the membership values of two sets
    println("Fuzzy Multiplication: " + fuzzyMultiplication(setA, setB))
    // calculates the absolute difference between membership values of two sets
    println("Fuzzy XOR: " + fuzzyXOR(setA, setB))
    // filters out elements with membership values less than the specified alpha
    println("Alpha Cut (0.6): " + alphaCut(setA, 0.6))

    // assign and eval normal gate
    assignGate("logicGate1", inputs => inputs.sum)
    val result1 = evaluateGate("logicGate1", List(0.5, 0.7))
    println(s"Evaluation of logicGate1 with inputs [0.5, 0.7]: ${result1.getOrElse("Evaluation Failed")}")

    // enter scope assign variables and check
    enterScope()
    assignVariable("A", 0.2)
    assignVariable("B", 0.8)
    println(s"Value of A in current scope: ${getVariable("A").getOrElse("Not Found")}")
    exitScope()
  }
}