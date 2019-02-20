package fpt.calculation

import java.time.LocalDate

import fpt.input._
import scalaz.{Cofree, Functor}
import matryoshka._
import matryoshka.implicits._
import scalaz._
import matryoshka.patterns._
import matryoshka.data._

import scala.language.higherKinds

object Enrichment extends App {

  case class Label(name: Option[String], prefix: List[String], time: Option[LocalDate])
  object Label {
    val default = Label(None, Nil, None)
  }
  type FSF     = Fix[RowF]
  type envt[A] = EnvT[Label, RowF, A]

  import FixPointTypes.rowFunctorImpl

  def lift: RowF[Cofree[RowF, Label]] => EnvT[Label, RowF, Cofree[RowF, Label]] = {
    case x: RowF[Cofree[RowF, Label]] => EnvT((Label.default, x))
  }

  def byTimeUnit(units: Seq[TimeAlignment]): Cofree[RowF, Label] => Cofree[RowF, Label] = {
    case Cofree(label, ParentRowF(a: AreaEntity, children: Seq[Cofree[RowF, Label]])) =>
      val byTime = units.map { unit =>
        val pairs = children.groupBy { case Cofree(l: Label, p: ParentRowF[Entity @unchecked, _]) =>
          unit(p.row.time)
        }.toSeq
        val withTime = pairs.flatMap { case (date, rows) =>
          rows.map { row =>
            Cofree(row.head.copy(time = Option(date)), row.tail)
          }
        }
        val label = Label(Option(unit.entryName), Nil, None)
        Cofree[RowF, Label](label, LevelRowF(withTime))
      }
      Cofree(label, ParentRowF(a, byTime))
    case other => other
  }

  def listAst(ast: FSF): Cofree[RowF, Label] = ast.transCata[Cofree[RowF, Label]][envt](lift)

  // def apply[G[_]: Functor]
  //          (f: F[U] => G[U])
  //          (implicit U: Corecursive.Aux[U, G], BF: Functor[F]): U

  lazy val lifted = listAst(EntitiesConsumingBoundary.shiftFPData)
  lazy val byTime = byTimeUnit(TimeAlignment.values)(lifted)
  // println(s"${lifted.head} , ${lifted.tail}")
  println(s"${byTime.head} , ${byTime.tail}")
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
