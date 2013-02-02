package progfun.week7


trait Stream[+T] {
  def isEmpty:Boolean
  def head: T
  def tail: Stream[T]
}

object Stream{
  def cons[T](hd:T, tl: => Stream[T]) = new Stream[T] {
    def isEmpty = false
    def head = hd
    lazy val tail = tl
  }

  val empty = new Stream[Nothing] {
    def isEmpty = true
    def head = throw new NoSuchElementException("stream.head")
    def tail = throw new NoSuchElementException("stream.tail")
  }
}
