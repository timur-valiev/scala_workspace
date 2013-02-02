object Main extends App {
  val in = new MyScanner

  def from(n:Int):scala.Stream[Int] = n #:: from(n+1)

  def sieve(s:Stream[Int]):Stream[Int] = {
    s.head #:: sieve(s.tail filter ( _ % s.head != 0))
  }

  val n = in.strings.head.toInt
  val nums = (in.strings.tail.take(n) map(_.toInt)).toList
  val primes = sieve(from(2)).takeWhile(_<=n).toList

  def getPrimes(x:Int):List[Int] = {
    val xx = primes.takeWhile(_<=math.sqrt(x)).filter(x %_ == 0)
    if(xx.isEmpty)
      List(x)
    else
      xx ::: (for{
        d <- xx
        if (x % d == 0 && x/d > d)
      } yield x/d)
  }


  def getAns(primes:List[Int], map1: Map[Int, Int]): Int = {
    (primes map (x=>map1(x))).max + 1
  }

  def makeMap(xs:List[Int]):Map[Int,Int] = xs match {
    case Nil => Map() withDefaultValue 0

    case x::t =>{
      val curPrimes = getPrimes(x)
      val ( prMap) = makeMap(t)
      val ans = getAns(curPrimes,prMap)
      ((curPrimes map (y=>y->ans)) foldLeft prMap)((z,y)=>z + y)}
  }

  println((makeMap(nums) foldLeft 0)((x,z)=>math.max(x,z._2)))


}




class MyScanner {
  def atoms(): Stream[String] = readLine().split(" ").toStream #::: atoms()
  val strings = atoms()
}