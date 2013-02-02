def nth[T](n:Int , l:List[T]): T = {
  if (l.isEmpty || n<0)
    throw new IndexOutOfBoundsException
  if (n == 0)
      l.head
  else
    nth[T](n-1, l.tail)
}

def last[T](xs: List[T]):T = xs match {
  case List() => throw new Error("last of empty list")
  case List(x) => x
  case y::ys => last(ys)
}

def init[T](xs: List[T]): List[T] = xs match {
  case List() => throw new Error("init of empty list")
  case List(x) => List()
  case y :: ys => y :: init(xs)
}

def reverse[T](xs: List[T]): List[T] = xs match {
  case List() => xs
  case y :: ys => reverse(ys) ::: List(y)
}

def concat[T](xs:List[T], ys:List[T]):List[T] = xs match {
  case List() => ys
  case z :: zs => z :: concat(zs, ys)
}

  def removeAt[T](n: Int, xs: List[T]): List[T] = xs match {
  case List() => xs
  case y :: ys => if (n == 0) ys else removeAt(n-1, ys)
}

def flatten(xs: List[Any]): List[Any] = xs match {
  case List() => xs
  case y::ys =>{
    val xxx = y match  {
      case List() => List()
      case z::zs => flatten(z::zs)
      case _ => List(y)
    }
    xxx ::: flatten(ys)
  }
}



def msort[T](xs: List[T])(implicit ord:Ordering[T]):List[T] = {
  val n = xs.length /2
  if (n==0)
    xs
  else {
    def merge(xs:List[T], ys:List[T]):List[T] = (xs,ys) match {
      case (List(),_) => ys
      case (_,List()) => xs
      case (x::xs1, y::ys1) => if (ord.lt(x,y)) x :: merge (xs1, ys) else y :: merge (xs, ys1)
    }
    val (l, r) = xs splitAt n
    merge(msort(l), msort(r))
  }
}

msort(List(1,2,4,3,6,3,7))

def squareList1(xs: List[Int]): List[Int] = xs match {
  case Nil     => xs
  case y :: ys => y*y :: squareList1(ys)
}

def squareList2(xs: List[Int]): List[Int] = xs map (x => x*x)

squareList1(List(1,2,4,3,6,3,7))
squareList2(List(1,2,4,3,6,3,7))

def pack[T](xs: List[T]): List[List[T]] = xs match {
  case Nil      => Nil
  case x :: xs1 => {
    val (first, last) = xs1 span(y => y == x)
    first :: pack(last)
  }
}
pack(List(1,1,2,2,2,3,1,1))

def encode1[T](xs:List[T]):List[(T,Int)] = xs match {
  case Nil => Nil
  case x :: xs1 => {
    val (first, last) = xs1 span(y => y == x)
    (x,first.length + 1) :: encode1(last)
  }
}

def encode2[T](xs:List[T]):List[(T,Int)] =
  pack(xs) map (ys => (ys.head, ys.length))

encode1(List(1,1,2,2,2,3,1,1))

def sum(xs: List[Int]) = (0 :: xs) reduceLeft (_ + _)
def product(xs: List[Int]) = (1 :: xs) reduceLeft (_ * _)
def reverse[T](xs: List[T]): List[T] = (xs foldLeft List[T]())((x,y)=>y::x)

reverse(List(1,2,3,4))