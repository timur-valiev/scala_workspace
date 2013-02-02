package progfun.week4

trait List[+T] {
  def isEmpty:scala.Boolean
  def head: T
  def tail: List[T]
  def prepend[U >: T](elem: U): List[U] = new Cons(elem, this)

}

class Cons[T](val head:T, val tail:List[T]) extends List[T]{
  def isEmpty = false
}

object Nil extends List[Nothing]{
  def isEmpty = true

  def head: Nothing = throw new NoSuchElementException("nil.head")

  def tail: Nothing = throw new NoSuchElementException("nil.tail")
}


object List{
  def apply = Nil
  def apply[T](x:T) = new Cons[T](x, Nil)
  def apply[T](x:T, y:T) = new Cons[T](x, new Cons[T](y, Nil))
}