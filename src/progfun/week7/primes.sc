def from(n:Int):scala.Stream[Int] = n #:: from(n+1)

val nats = from(0)
val m4s = nats map (_ * 4)
(m4s take 100).toList






def sieve(s:Stream[Int]):Stream[Int] = {
  s.head #:: sieve(s.tail filter ( _ % s.head != 0))
}
(sieve(from(2)) take 100).toList






def sqrtStream(x:Double):Stream[Double] = {
  def improve(guess:Double):Double = (guess + x/guess)/2
  lazy val guesses: Stream[Double] = 1 #:: (guesses map improve)
  guesses
}



(sqrtStream(16) take 20).toList












