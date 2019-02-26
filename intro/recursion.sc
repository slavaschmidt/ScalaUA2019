class Scope[A] {
  type Fa = A => A
  type Ft = Fa => Fa
  val fix: Ft => Fa = f => a => f(fix(f))(a)
}
def fac(f: Long => Long)(n: Long) = if (n < 3) n else n * f(n - 1)
def factorial(n: Long)            = new Scope[Long].fix(fac)(n)

println(factorial(10))

import scala.language.higherKinds

case class Fix[F[_]](unfix: F[Fix[F]])

// ------------------------ LIST ------------------------
sealed trait FList[+H, +T]
case class FCons[H, T](h: H, t: T) extends FList[H, T]
sealed trait FNil extends FList[Nothing, Nothing]
case object FNil extends FNil

def nil[A]                             = Fix[FList[A, ?]](FNil)
def cons[A](h: A, t: Fix[FList[A, ?]]) = Fix[FList[A, ?]](FCons(h, t))

val list: Fix[FList[Int, ?]] = cons(1, cons(2, cons(3, nil)))

println(list)

trait Inductive
trait INil extends Inductive
case class IFix[F[_], R <: Inductive](f: F[R]) extends Inductive

type HNil = IFix[FList[FNil, ?], Nothing]

type ::[A, R <: Inductive] = IFix[FList[A, ?], R]

val hnil: HNil = IFix[FList[FNil, ?], Nothing](FNil)

def hcons[A, R <: Inductive](x: A, xs: R): A :: R =
  IFix[FList[A, ?], R](FCons(x, xs))

val hs: Int :: String :: Symbol :: HNil =
  hcons(1, hcons("one", hcons('three, hnil)))

val ls = hcons(1, hcons(2, hcons(3, hnil)))

println(hs)

println(ls)

trait TreeF[+L, +N]
case class LeafF[L, N](l: L)       extends TreeF[L, Nothing]
case class NodeF[L, N](l: N, r: N) extends TreeF[L, N]

type Tree[L] = Fix[TreeF[L, ?]]

def leaf[L](l: L): Tree[L] = Fix[TreeF[L, ?]](LeafF(l))
def node[L](l: Tree[L], r: Tree[L]): Tree[L] =
  Fix[TreeF[L, ?]](NodeF(l, r))
