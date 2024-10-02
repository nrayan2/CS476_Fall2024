object FuzzyLogic {
  case class FuzzyElement(name: String, membership: Double)

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

  // fuzzy additon between 2 fuzzy sets
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
