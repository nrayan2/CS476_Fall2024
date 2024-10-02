import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import FuzzyLogicGates.{evaluateGate => evalGateFromFuzzyLogicGates}
import GateOperations.{assignGate, evaluateGate, enterScope, exitScope, assignVariable, getVariable, testGate}
import FuzzyLogic._

class FuzzyLogicTest extends AnyFlatSpec with Matchers {

  behavior of "Fuzzy Logic DSL"

  it should "perform fuzzy XOR" in {
    val setA = List(FuzzyLogic.FuzzyElement("x1", 0.2), FuzzyLogic.FuzzyElement("x2", 0.7), FuzzyLogic.FuzzyElement("x3", 0.5))
    val setB = List(FuzzyLogic.FuzzyElement("x1", 0.6), FuzzyLogic.FuzzyElement("x2", 0.3), FuzzyLogic.FuzzyElement("x3", 0.5))
    val result = FuzzyLogic.fuzzyXOR(setA, setB)

    result.zip(List(FuzzyLogic.FuzzyElement("x1", 0.4), FuzzyLogic.FuzzyElement("x2", 0.4), FuzzyLogic.FuzzyElement("x3", 0.0)))
      .foreach { case (resultElem, expectedElem) =>
        resultElem.name shouldBe expectedElem.name
        resultElem.membership shouldEqual expectedElem.membership +- 1e-9
      }
  }

  it should "perform fuzzy union" in {
    val setA = List(FuzzyElement("x1", 0.2), FuzzyElement("x2", 0.7))
    val setB = List(FuzzyElement("x1", 0.5), FuzzyElement("x2", 0.6))
    val result = fuzzyUnion(setA, setB)

    result shouldEqual List(FuzzyElement("x1", 0.5), FuzzyElement("x2", 0.7))
  }

  it should "perform fuzzy intersection" in {
    val setA = List(FuzzyElement("x1", 0.2), FuzzyElement("x2", 0.7))
    val setB = List(FuzzyElement("x1", 0.5), FuzzyElement("x2", 0.6))
    val result = fuzzyIntersection(setA, setB)

    result shouldEqual List(FuzzyElement("x1", 0.2), FuzzyElement("x2", 0.6))
  }

  it should "perform fuzzy complement" in {
    val setA = List(FuzzyElement("x1", 0.2), FuzzyElement("x2", 0.7))
    val result = fuzzyComplement(setA)

    result.zip(List(FuzzyElement("x1", 0.8), FuzzyElement("x2", 0.3)))
      .foreach { case (resultElem, expectedElem) =>
        resultElem.name shouldBe expectedElem.name
        resultElem.membership shouldEqual expectedElem.membership +- 1e-9
      }
  }

  it should "perform fuzzy addition" in {
    val setA = List(FuzzyElement("x1", 0.4), FuzzyElement("x2", 0.7))
    val setB = List(FuzzyElement("x1", 0.5), FuzzyElement("x2", 0.6))
    val result = fuzzyAddition(setA, setB)

    result shouldEqual List(FuzzyElement("x1", 0.9), FuzzyElement("x2", 1.0))
  }

  it should "perform fuzzy multiplication" in {
    val setA = List(FuzzyElement("x1", 0.4), FuzzyElement("x2", 0.7))
    val setB = List(FuzzyElement("x1", 0.5), FuzzyElement("x2", 0.6))
    val result = fuzzyMultiplication(setA, setB)

    result shouldEqual List(FuzzyElement("x1", 0.2), FuzzyElement("x2", 0.42))
  }

  it should "perform alpha cut" in {
    val setA = List(FuzzyElement("x1", 0.3), FuzzyElement("x2", 0.7), FuzzyElement("x3", 0.8))
    val result = alphaCut(setA, 0.6)

    result shouldEqual List(FuzzyElement("x2", 0.7), FuzzyElement("x3", 0.8))
  }

  it should "assign and eval gates" in {
    assignGate("testGate", inputs => inputs.sum)
    val result = evaluateGate("testGate", List(0.3, 0.4)).getOrElse(0.0)
    result shouldBe 0.7
  }

  it should "handle scopes and assign variable" in {
    enterScope()
    assignVariable("X", 0.5)
    getVariable("X") shouldBe Some(0.5)
    exitScope()
  }

  it should "test gates with expected vals" in {
    assignGate("testGateXOR", inputs => inputs.reduce(_ - _))
    testGate("testGateXOR", List(0.3, 0.6), -0.3) shouldBe true
  }

  it should "handle missing inputs for gate eval" in {
    assignGate("incompleteGate", inputs => if (inputs.isEmpty) 0.0 else inputs.sum)
    val result = evaluateGate("incompleteGate", List()).getOrElse(0.0)
    result shouldBe 0.0
}


}
