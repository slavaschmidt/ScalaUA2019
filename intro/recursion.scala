import scala.{List => _}
object App {

  class Scope[A] {
    type FA = A  => A
    type FT = FA => FA

    val rec: FT => FA = (f: FT) => (a: A) => f(rec(f))(a)
  }

  import scala.language.higherKinds

  case class Rec[F[_]](f: F[Rec[F]])

  trait ListF[+H, +T]
  trait NilF extends ListF[Nothing, Nothing]
  case object NilF extends NilF
  case class ConsF[H, T](h: H, t: T) extends ListF[H, T]

  type List[A] = Rec[ListF[A, ?]]

  def nil[A]: List[A] = Rec[ListF[A, ?]](NilF)

  def cons[A](h: A, t: List[A]): List[A] = Rec[ListF[A, ?]](ConsF(h, t))

  trait TreeLike[+L, +N]
  case class LeafLike[L, N](l: L) extends TreeLike[L, Nothing]
  case class NodeLike[L, N](l: N, r: N) extends TreeLike[L, N]

  type Tree[L] = Rec[TreeLike[L, ?]]

  def leaf[L](l: L): Tree[L] = Rec[TreeLike[L, ?]](LeafLike(l))
  def node[L](l: Tree[L], r: Tree[L]): Tree[L] =
    Rec[TreeLike[L, ?]](NodeLike(l, r))

  case class HRec[F[_], R](f: F[R])

  type HNil     = HRec[ListF[NilF, ?], Nothing]
  type ::[A, R] = HRec[ListF[A, ?], R]

  def hnil: HNil                      = HRec[ListF[NilF, ?], Nothing](NilF)
  def hcons[A, R](h: A, t: R): A :: R = HRec[ListF[A, ?], R](ConsF(h, t))

  def main(args: Array[String]): Unit = {
    val fact = (f: Int => Int) => (n: Int) => if (n < 3) n else n * f(n - 1)

    def factorial(n: Long) = new Scope[Long].rec(fact)(n)

    println(factorial(15))
    val list = cons(1, cons(2, cons(3, nil)))
    val tree =
      node(node(node(leaf("B"), leaf("C")), leaf("E")), leaf("D"))
    val hl: Int :: String :: Char :: HNil =
      hcons(1, hcons("A", hcons('3', hnil)))
    println(list)
    println(tree)
    println(hl)
    // list match {
    //   case Rec(ConsF(h, t)) => println(s"Cons: $h and $t")
    //   case Rec(NilF) => println("It's nil")
    // }

    // hl match {
    //   case HRec(ConsF(h, t)) => println(s"Cons: $h and $t")
    //   case HRec(NilF) => println("It's nil")
    // }

  }
}
