package progfun.week5_6

class Polynom(terms0: Map[Int, Double]) {
  def this(bindings:(Int, Double)*) = this(bindings.toMap)
  val terms = terms0 withDefaultValue 0.0
  //def +(other:Polynom) = new Polynom(terms ++ (other.terms map adjust))
  def adjust(term:(Int, Double)):(Int, Double) = {
    val (exp,coef) = term
    exp -> (coef + terms(exp))
  }

  def + (other: Polynom) =
    new Polynom((other.terms foldLeft terms)(addTerm))

  def addTerm(terms: Map[Int, Double], term: (Int, Double)) = {
    val (exp, coef) = term
    terms + ( exp -> (coef+terms(exp)))
  }

  override def toString = (for ((exp, coef)<-terms.toList.sorted.reverse) yield coef+"x^"+exp) mkString(" + ")

}
