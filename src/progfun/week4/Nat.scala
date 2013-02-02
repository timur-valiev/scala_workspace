package progfun.week4

abstract class Nat {
  def isZero: scala.Boolean
  def predecessor: Nat
  def successor: Nat = new Succ(this)
  def + (that: Nat): Nat
  def - (that: Nat): Nat
}

object Zero extends Nat{
  def isZero = true

  def predecessor: Nat = throw new Error("hasnt prev")

  def +(that: Nat): Nat = that

  def -(that: Nat): Nat = if (that.isZero) Zero else throw new Error
}

class Succ(n: Nat) extends Nat{
  def isZero = false

  def predecessor: Nat = n

  def +(that: Nat): Nat = new Succ(n + that)

  def -(that: Nat): Nat = if (that.isZero) this else n - that.predecessor
}
