package progfun.week4

trait Expr {
  def eval:Int = this match {
    case Number(n) => n
    case Sum(e1, e2) => e1.eval + e2.eval
    case Prod(e1, e2) => e1.eval * e2.eval
  }
  def show:String = this match {
    case Number(n) => n.toString
    case Var(s) => s
    case Sum(e1, e2) => e1.show + " + "+ e2.show
    case Prod(e1,e2) => (e1 match {case Sum(e3,e4) => "( " + e1.show + " )"  case _ => e1.show}) + " * " +
                        (e2 match {case Sum(e3,e4) => "( " + e2.show + " )"  case _ => e2.show} )
  }
}
case class Number(n:Int) extends Expr
case class Var(s:String) extends Expr
case class Sum(e1:Expr, e2:Expr) extends Expr
case class Prod(e1:Expr, e2:Expr) extends Expr

