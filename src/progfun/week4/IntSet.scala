package progfun.week4

abstract class IntSet {
  def incl(x: Int): IntSet

  def contains(x: Int): scala.Boolean

  def union(other: IntSet): IntSet
}

object Empty extends IntSet {
  def incl(x: Int): IntSet = new NotEmpty(x, Empty, Empty)

  def contains(x: Int) = false

  def union(other: IntSet): IntSet = other
}

class NotEmpty(elem: Int, right: IntSet, left: IntSet) extends IntSet {
  def incl(x: Int): IntSet =
    if (x < elem)
      new NotEmpty(elem, left.incl(x), right)
    else if (x > elem) new NotEmpty(elem, left, right.incl(x)) else this

  def contains(x: Int) =
    if (x < elem)
      left.contains(x)
    else
      if (x > elem) right.contains(x) else true

  def union(other: IntSet): IntSet = other union left union right incl elem
}



