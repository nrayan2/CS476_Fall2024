import FuzzyLogic._
import FuzzyLogicGates.{defineClass, createInstance, evaluateGate => evalGateFromFuzzyLogicGates}
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

    // define new class
    defineClass("exampleClass", "methodTest", List("input1", "input2"), (inputs: Map[String, Any]) => 
      inputs.values.map(_.asInstanceOf[Double]).sum
    )

    // create instance
    val instance = createInstance("exampleClass")

    // add variable to instance
    instance.addVariable("input", 0.5)
    println(s"Variable 'input' in instance: ${instance.getVariable("input")}")
  }
}