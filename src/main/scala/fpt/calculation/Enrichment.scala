package fpt.calculation

import java.time.LocalDate

import fpt.input.{EntitiesConsumingBoundary, ParentRowF, RowF}
import scalaz.{Cofree, Functor}
import matryoshka._
import matryoshka.implicits._
import scalaz._
import matryoshka.patterns._
import matryoshka.data._

import scala.language.higherKinds

object Enrichment {

  case class Label(name: Option[String], prefix: List[String], time: Option[LocalDate])
  object Label {
    val default = Label(None, Nil, None)
  }
  type FSF     = Fix[RowF]
  type envt[A] = EnvT[Label, RowF, A]

  implicit val sourceFunctorImpl: Functor[RowF] = new Functor[RowF] {
    override def map[A, B](a: RowF[A])(f: A => B): RowF[B] = a match {
      case ParentRowF(n, c) => ParentRowF(n, c.map(f))
    }
  }

  def lift: RowF[Cofree[RowF, Label]] => EnvT[Label, RowF, Cofree[RowF, Label]] = {
    case x: RowF[Cofree[RowF, Label]] => EnvT((Label.default, x))
  }

  def listAst(ast: FSF): Cofree[RowF, Label] = ast.transCata[Cofree[RowF, Label]][envt](lift)

  // def apply[G[_]: Functor]
  //          (f: F[U] => G[U])
  //          (implicit U: Corecursive.Aux[U, G], BF: Functor[F]): U

  println(listAst(EntitiesConsumingBoundary.shiftFPData))
}
/*

case class Labeled[S[_], A](label: A, source: S[Labeled[S, A]])

def liftA: SourceF[Labeled[SourceF, Label]] => Labeled[SourceF, Label] = {
  case x => Labeled(Label.default, x)
}

type LabeledSource[A] = Labeled[SourceF, A]

implicit val labeledFunctorImpl: Functor[LabeledSource] = new Functor[LabeledSource] {
  override def map[A, B](a: LabeledSource[A])(f: A => B): LabeledSource[B] = a match {
    case Labeled(l ,s) =>
      val b: B = f(l)
      Labeled(b, sourceFunctorImpl.map(s)(f))
      ???
  }
}

  lazy val labeledCoalgebra: Coalgebra[Labeled, A] =
    _.map {
      case Labeled(l, s) =>
    }.asJson

    def liftInput(ast: FSF): Labeled[SourceF, Label] =  ast.transCata[Labeled[SourceF, Label]][LabeledSource](liftA)

 */
