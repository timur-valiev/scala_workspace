def isPrime(n:Int):Boolean =
  !(2 to n-1).exists(x => n % x == 0)

def scalarProduct(xs:List[Int], ys:List[Int]):Int =
 (for {
   (x,y) <- xs zip ys
 } yield x*y).sum


def queens(n: Int): Set[List[Int]] = {
  def checkQueens(prev: List[Int], col: Int): Boolean = {
    val row = prev.length
    val q = (row - 1 to 0 by -1) zip prev
    q forall  {case (x,y) => y!=col && (row - x) != math.abs(y-col)}
  }

  def placeQueens(k:Int) : Set[List[Int]] ={
    if (k==0) Set(List())
    else
      for {
        prev <-  placeQueens(k-1)
        col <- 0 until n
        if checkQueens(prev, col)
      } yield col::prev
  }
  placeQueens(n)
}
queens(4)




