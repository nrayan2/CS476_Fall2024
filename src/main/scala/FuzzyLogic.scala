object FuzzyLogic {
  case class FuzzyElement(name: String, membership: Double)

  def fuzzyUnion(setA: List[FuzzyElement], setB: List[FuzzyElement]): List[FuzzyElement] = {
    setA.zip(setB).map { case (a, b) => FuzzyElement(a.name, math.max(a.membership, b.membership)) }
  }

  def fuzzyIntersection(setA: List[FuzzyElement], setB: List[FuzzyElement]): List[FuzzyElement] = {
    setA.zip(setB).map { case (a, b) => FuzzyElement(a.name, math.min(a.membership, b.membership)) }
  }

  def fuzzyComplement(set: List[FuzzyElement]): List[FuzzyElement] = {
    set.map(e => FuzzyElement(e.name, 1 - e.membership))
  }

  def fuzzyAddition(setA: List[FuzzyElement], setB: List[FuzzyElement]): List[FuzzyElement] = {
    setA.zip(setB).map { case (a, b) => FuzzyElement(a.name, math.min(1, a.membership + b.membership)) }
  }

  def fuzzyMultiplication(setA: List[FuzzyElement], setB: List[FuzzyElement]): List[FuzzyElement] = {
    setA.zip(setB).map { case (a, b) => FuzzyElement(a.name, a.membership * b.membership) }
  }

  def fuzzyXOR(setA: List[FuzzyElement], setB: List[FuzzyElement]): List[FuzzyElement] = {
    setA.zip(setB).map { case (a, b) => FuzzyElement(a.name, math.abs(a.membership - b.membership)) }
  }

  def alphaCut(set: List[FuzzyElement], alpha: Double): List[FuzzyElement] = {
    set.filter(_.membership >= alpha)
  }
}
