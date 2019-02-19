package fpt.result

import java.time.{LocalDate, ZonedDateTime}

import fpt.input.Entity
import io.circe.Json
import io.circe.syntax._
import matryoshka._
import matryoshka.implicits._
import scalaz._
import matryoshka.patterns._
import matryoshka.data._

import scala.language.higherKinds

sealed trait ResultF[+A]
final case class SliceF[A](mappings: Map[String, A]) extends ResultF[A]
final case class ChartF[A](points: Seq[A]) extends ResultF[A]
final case class PointF(time: LocalDate, data: Map[String, Double]) extends ResultF[Nothing]

sealed trait SourceF[+A]
final case class Node[A](name: String, children: Seq[A]) extends SourceF[A]

object ResultFixPointTypes extends App {

  implicit val rowFunctorImpl: Functor[ResultF] = new Functor[ResultF] {
    override def map[A, B](a: ResultF[A])(f: A => B): ResultF[B] = a match {
      case SliceF(m) => SliceF(m.mapValues(f))
      case ChartF(p) => ChartF(p.map(f))
      case PointF(t, d) => PointF(t, d)
    }
  }

  val resultToJsonAlgebra: Algebra[ResultF, Json] = {
    case PointF(t, d) =>
      d.mapValues(_.asJson).updated("__time__", t.asJson).asJson

    case ChartF(p) =>
      val fields: Seq[String] = p.flatMap(_.asObject.toSeq.flatMap(_.keys.toSeq)).distinct.sorted
      Map("__data__" -> p.asJson, "__fields__" -> fields.asJson).asJson

    case SliceF(mappings) =>
      mappings.asJson
  }

  def resultToJson[T](result: T)(implicit raux: Recursive.Aux[T, ResultF]) =
    result.cata[Json](resultToJsonAlgebra)

  def result[T](implicit caux: Corecursive.Aux[T, ResultF]): T =
    SliceF(
      Map(
        "colors" ->
          SliceF(
            Map(
              "red" ->
                ChartF(
                  Seq(
                    PointF(LocalDate.now(), Map("a"  -> 1d, "b"  -> 2d)).embed,
                    PointF(LocalDate.now(), Map("aa" -> 1d, "bb" -> 2d)).embed
                  )
                ).embed,
              "green" -> ChartF(Seq(PointF(LocalDate.now(), Map("c" -> 2d, "d" -> 4d)).embed)).embed,
              "blue"  -> ChartF(Seq(PointF(LocalDate.now(), Map("e" -> 3d, "f" -> 6d)).embed)).embed
            )
          ).embed
      )
    ).embed

  val json = resultToJson(result[Fix[ResultF]])

  println(json.asJson)
  // type Algebra[F[_], A]               = F[A] => A
  // type Coalgebra[F[_], A]             = A => F[A]    // GCoalgebra[Id, F, A]
  // def hylo[F[_]: Functor, A, B](a: A)(φ: Algebra[F, B], ψ: Coalgebra[F, A]): B =

}

object IntermediateModel {
  type Rows[E <: Entity] = Seq[E]
  type FN[E <: Entity]   = Rows[E] => Double

  case class SliceMeta[E <: Entity](
                                     time: ZonedDateTime,
                                     path: List[String], // this won't work, we need a tree
                                     prefix: List[String],
                                     fs: Map[String, FN[E]]
                                   )

  case class Data[E <: Entity](data: Rows[E], meta: SliceMeta[E])

  def rowsToResultCoalgebra[E <: Entity]: Coalgebra[ResultF, Map[String, Data[E]]] = {
    case data if data.values.forall(_.meta.path.isEmpty) =>
      val values: Seq[Map[String, Data[E]]] = ???
      ChartF(values)
    case data =>
      val mappings: Map[String, Map[String, Data[E]]] = ???
      SliceF(mappings)
    /*
        case data =>
          val prefix = meta.prefix.mkString("","/","/")
          val r = meta.fs.map { case (name, f) =>  s"$prefix$name" -> f(data) }
          PointF(meta.time.toLocalDate, r)
        case data =>
          val points: Seq[_] = ???
          ChartF(points)
    */
  }


  // case  => SliceF(m.mapValues(f))
  // case ChartF(p) => ChartF(p.map(f))


}
