import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import FuzzyLogicGates.{defineClass, createInstance, evaluateGate => evalGateFromFuzzyLogicGates}
import GateOperations.{createClass, assignGate, evaluateGate, enterScope, exitScope, assignVariable, getVariable, testGate}
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

  it should "test gates w expected vals" in {
    assignGate("testGateXOR", inputs => inputs.reduce(_ - _))
    testGate("testGateXOR", List(0.3, 0.6), -0.3) shouldBe true
  }

  it should "handle missing inputs for gate eval" in {
    assignGate("incompleteGate", inputs => if (inputs.isEmpty) 0.0 else inputs.sum)
    val result = evaluateGate("incompleteGate", List()).getOrElse(0.0)
    result shouldBe 0.0
  }

  behavior of "Fuzzy logic DSL w classes and inheritance"

  it should "define and invoke a method on a class instance" in {
    // define class w method that takes two params and sums
    defineClass("SimpleClass", "sumMethod", List("a", "b"), params => 
      params("a").asInstanceOf[Double] + params("b").asInstanceOf[Double]
    )

    // create instance and invoke method
    val instance = createInstance("SimpleClass")
    val result = instance.InvokeMethod("sumMethod", Map("a" -> 3.5, "b" -> 2.5))
    result shouldBe 6.0
  }

  it should "support variable shadowing in subclasses" in {
    // define base and derived classes w same variable
    defineClass("BaseClassCopy", "getVar", List(), _ => 1)
    val base = createInstance("BaseClassCopy")
    base.addVariable("x", 10)

    defineClass("DerivedClassCopy", "getVar", List(), _ => 2)
    val derived = createInstance("DerivedClassCopy")
    derived.addVariable("x", 20)

    // check variable shadowing
    base.getVariable("x") shouldBe Some(10)
    derived.getVariable("x") shouldBe Some(20)
  }

  it should "allow nested classes" in {
    // define class w nested class
    defineClass("OuterClass", "outerMethod", List(), _ => "outer result")
    defineClass("OuterClass.InnerClass", "innerMethod", List(), _ => "inner result")

    // create instances and invoke method
    val outer = createInstance("OuterClass")
    val inner = createInstance("OuterClass.InnerClass")

    val outerResult = outer.InvokeMethod("outerMethod", Map())
    val innerResult = inner.InvokeMethod("innerMethod", Map())

    outerResult shouldBe "outer result"
    innerResult shouldBe "inner result"
  }

  it should "define and evaluate custom gates" in {
    // define custom gate
    assignGate("customSumGate", inputs => inputs.sum)

    // evaluate the gate w sample inputs
    val gateResult = evaluateGate("customSumGate", List(1.5, 2.5, 3.0))
    gateResult shouldBe Some(7.0)
  }

  it should "create instances and work w custom methods and variables" in {
    // define class w method that calculates area of rectangle
    defineClass("Rectangle", "calculateArea", List("length", "width"), params => 
      params("length").asInstanceOf[Double] * params("width").asInstanceOf[Double]
    )

    val rect = createInstance("Rectangle")
    rect.addVariable("length", 5.0)
    rect.addVariable("width", 3.0)

    // check calculated area
    val area = rect.InvokeMethod("calculateArea", Map("length" -> 5.0, "width" -> 3.0))
    area shouldBe 15.0
  }

  it should "support dynamic dispatch based on the class hierarchy" in {
    // define base class
    defineClass("cat", "sound", List(), _ => "meow")

    // define subclass
    defineClass("dog", "sound", List(), _ => "bark")

    val animal = createInstance("cat")
    val dog = createInstance("dog")

    animal.InvokeMethod("sound", Map()) shouldBe "meow"
    dog.InvokeMethod("sound", Map()) shouldBe "bark"
  }

  it should "handle missing methods gracefully" in {
    // invoke method that DNE
    defineClass("emptyClass", "deadMethod", List(), _ => "dead")
    val instance = createInstance("emptyClass")

    // method DNE
    intercept[NoSuchElementException] {
      instance.InvokeMethod("DNEMethod", Map())
    }
  }
}
