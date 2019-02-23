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
    def named(name: String) = Label(Option(name), Nil, None)
  }

  type envt[A] = EnvT[Label, RowF, A]

  def lift: RowF[Cofree[RowF, Label]] => EnvT[Label, RowF, Cofree[RowF, Label]] = {
    case x: RowF[Cofree[RowF, Label]] => EnvT((Label.default, x))
  }

  // this is a function which only enriches the top level
  def byTimeUnitFn(units: Seq[TimeAlignment]): Cofree[RowF, Label] => Cofree[RowF, Label] = {
    case Cofree(label, ParentRowF(a: AreaEntity, children: Seq[Cofree[RowF, Label]])) =>
      val byTime = units.map { unit =>
        val pairs = children.groupBy {
          case Cofree(l: Label, p: ParentRowF[Entity @unchecked, _]) =>
            unit(p.row.time)
        }.toSeq
        val withTime = for {
          (date, rows) <- pairs
          row <- rows
        } yield Cofree(row.head.copy(time = Option(date)), row.tail)
        val label = Label.named(unit.entryName)
        Cofree[RowF, Label](label, LevelRowF(withTime))
      }
      Cofree(label, ParentRowF(a, byTime))
    case other => other
  }

  // this is a function which creates additional layer for shifts
  val byShiftName: Cofree[RowF, Label] => Cofree[RowF, Label] = {
    case Cofree(label, ParentRowF(a: AreaEntity, children: Seq[Cofree[RowF, Label]])) =>
        val byShift = children.groupBy {
          case Cofree(_, p: ParentRowF[ShiftEntity @unchecked, _]) => p.row.name
        }.toSeq
        val ch = byShift.map {
          case (shiftName, seq) => Cofree(Label.named(shiftName), LevelRowF(seq))
        }
        val nested: Seq[Cofree[RowF, Label]] = Seq(
          Cofree(Label.named("Compare"), LevelRowF(ch)),
          Cofree(Label.named("Sum"), LevelRowF(children))
        )
        Cofree(label, LevelRowF(nested))
    case other => other
  }


  // use EnvT.hmap or EnvT.traverse
  def byTimeUnitNat(units: Seq[TimeAlignment]): EnvT[Label, RowF, Cofree[RowF, Label]] => EnvT[Label, RowF, Cofree[RowF, Label]] = {
    case x @ EnvT((l, r)) => r match {
      case ParentRowF(a: AreaEntity, children: Seq[Cofree[RowF, Label]]) =>
        val byTime = units.map { unit =>
          val pairs = children.groupBy {
            case Cofree(l: Label, p: ParentRowF[Entity @unchecked, _]) =>
              unit(p.row.time)
          }.toSeq
          val withTime = for {
            (date, rows) <- pairs
            row <- rows
          } yield Cofree(row.head.copy(time = Option(date)), row.tail)
          val label = Label(Option(unit.entryName), Nil, None)
          Cofree[RowF, Label](label, LevelRowF(withTime))
        }
        EnvT((l, ParentRowF(a, byTime)))
      case ParentRowF(e, children: Seq[Cofree[RowF, Label]]) =>
        val ch = children.map { case Cofree(label, row) =>
            Cofree(label.copy(time = l.time), row)
        }
        EnvT((l, ParentRowF(e, ch)))
      case LevelRowF(children: Seq[Cofree[RowF, Label]]) =>
        val ch = children.map { case Cofree(label, row) =>
            Cofree(label.copy(time = l.time), row)
        }
        EnvT((l, LevelRowF(ch)))

    }
  }

  import FixPointTypes.rowFunctorImpl

  // def apply[U] = new PartiallyApplied[U] {
  //   def apply[G[_]: Functor]
  //          (f: F[U] => G[U])
  //          (implicit U: Corecursive.Aux[U, G], BF: Functor[F]): U
  // }

  /*
    U = Cofree[RowF, Label]
    G[_] = envt = EnvT[Label, RowF, _]
    f: F[U] => G[U] = RowF[Cofree[RowF, Label]] => EnvT[Label, RowF, Cofree[RowF, Label]]
    F[_] = Fix[_]
  */
  val listAst: (Fix[RowF]) => Cofree[RowF, Label] = _.transCata[Cofree[RowF, Label]][envt](lift)

  /*
    U = Cofree[RowF, Label]
    f: F[U] => G[U] = EnvT[Label, RowF, Cofree[RowF, Label]] => EnvT[Label, RowF, Cofree[RowF, Label]]
    F = G = EnvT[Label, RowF, _]
  */
  def byTimeUnitRec(units: Seq[TimeAlignment]): Cofree[RowF, Label] => Cofree[RowF, Label] = 
    _.transCata[Cofree[RowF, Label]][envt](byTimeUnitNat(units))

  lazy val lifted = listAst(EntitiesConsumingBoundary.shiftFPData)

  lazy val byTimeSimple = byTimeUnitRec(TimeAlignment.values)(lifted)

  lazy val fullChain = listAst andThen byShiftName andThen byTimeUnitRec(TimeAlignment.values)
  
  lazy val fullChainBroken = listAst andThen byTimeUnitRec(TimeAlignment.values) andThen byShiftName

  lazy val byTime  = fullChain(EntitiesConsumingBoundary.shiftFPData)

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
