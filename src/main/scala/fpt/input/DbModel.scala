package fpt.input

import java.util.UUID

import io.circe.{Encoder, Json}

import scala.language.higherKinds
import scalaz._
import matryoshka.{hylo, _}
import matryoshka.data.Fix
import matryoshka.implicits._

sealed trait RowF[+A]
final case class ParentRowF[E <: Entity, B](row: E, children: Seq[B]) extends RowF[B]
// STEP 2: eliminate this as well // final case class BottomRowF[E <: Entity, B](row: E) extends RowF[B]

// final case class AreaF[A](id: UUID, children: Seq[A])
// final case class ShiftF[A](shift: ShiftEntity, children: Seq[A]) extends RowF[+A]
// final case class GradeF[A](grade: GradeEntity, children: Seq[A]) extends RowF[+A]
// final case class OrderF[A](shift: OrderEntity, children: Seq[A]) extends RowF[+A]
// final case class StopF[A](stop: StopEntity,    children: Seq[A]) extends RowF[+A]

object FixPointTypes {

  implicit val rowFunctorImpl: Functor[RowF] = new Functor[RowF] {
    override def map[A, B](a: RowF[A])(f: A => B): RowF[B] = a match {
      case ParentRowF(r, d) => ParentRowF(r, d.map(f))
      // step 2 , eliminate // case BottomRowF(r) => BottomRowF(r)
    }
  }

  type EntitiesInput = (
    Seq[AreaEntity],
    Seq[(UUID, ShiftEntity)],
    Seq[(UUID, UUID, GradeEntity)],
    Seq[(UUID, UUID, UUID, OrderEntity)],
    Seq[(UUID, UUID, UUID, UUID, StopEntity)]
  )
}
import FixPointTypes.EntitiesInput

object EntitiesConsumingBoundary extends App {

  // type Coalgebra[F[_], A]             = A => F[A]

  lazy val entitiesCoalgebraFull: Coalgebra[RowF, EntitiesInput] = {
    case (Nil, Nil, Nil, Nil, Seq(stop)) =>
      ParentRowF(stop._5, Nil)

    case (Nil, Nil, Nil, Seq(order), stops) =>
      var pushDown = stops.map { s =>
        (Nil, Nil, Nil, Nil, Seq(s))
      }
      ParentRowF(order._4, pushDown)

    case (Nil, Nil, Seq(grade), orders, stops) =>
      var pushDown = orders.map { o =>
        (Nil, Nil, Nil, Seq(o), stops.filter(_._4 == o._4.id))
      }
      ParentRowF(grade._3, pushDown)

    case (Nil, Seq(shift), grades, orders, stops) =>
      var pushDown = grades.map { g =>
        (Nil, Nil, Seq(g), orders.filter(_._3 == g._3.id), stops.filter(_._3 == g._3.id))
      }
      ParentRowF(shift._2, pushDown)
    case (Seq(area), shifts, grades, orders, stops) =>
      var pushDown = shifts.map { s =>
        (Nil, Seq(s), grades.filter(_._2 == s._2.id), orders.filter(_._2 == s._2.id), stops.filter(_._1 == s._2.id))
      }
      ParentRowF(area, pushDown)
  }

  lazy val entitiesCoalgebraShort: Coalgebra[RowF, EntitiesInput] = {
    case (Nil, Nil, Nil, Nil, Seq(stop)) =>
      ParentRowF(stop._5, Nil)

    case (Nil, Seq(shift), grades, orders, stops) =>
      var pushDown = stops.map { s =>
        (Nil, Nil, Nil, Nil, stops.filter(_._1 == shift._2.id))
      }
      ParentRowF(shift._2, pushDown)

    case (Seq(area), shifts, grades, orders, stops) =>
      var pushDown = shifts.map { s =>
        (Nil, Seq(s), grades.filter(_._2 == s._2.id), orders.filter(_._2 == s._2.id), stops.filter(_._1 == s._2.id))
      }
      ParentRowF(area, pushDown)
  }

  def toShiftStops(
    area: AreaEntity,
    shifts: Seq[(UUID, ShiftEntity)],
    grades: Seq[(UUID, UUID, GradeEntity)],
    orders: Seq[(UUID, UUID, UUID, OrderEntity)],
    stops: Seq[(UUID, UUID, UUID, UUID, StopEntity)]
  )(implicit F: Functor[RowF]) =
    (Seq(area), shifts, grades, orders, stops).ana[Fix[RowF]](entitiesCoalgebraShort)

  import DbData._
  import FixPointTypes.rowFunctorImpl

  lazy val shiftFPData: Fix[RowF] = toShiftStops(area, shiftsData, Nil, Nil, stopsData)

  println(shiftFPData)
}

import EntitiesConsumingBoundary._

object EntitiesProducingBoundary extends App {

  import io.circe.Json
  import io.circe.syntax._
  import io.circe.generic.auto._

  implicit val erf: Encoder[RowF[Json]] = (a: RowF[Json]) => a.asJson

  lazy val entitiesAlgebra: Algebra[RowF, Json] = {
    case ParentRowF(row, children) => Map("row" -> row.asJson, "children" -> children.asJson).asJson
  }

  import FixPointTypes.rowFunctorImpl

  def toJson[T](entity: T)(implicit r: Recursive.Aux[T, RowF]): Json =
    entity.cata[Json](entitiesAlgebra)

  println(toJson(shiftFPData))

}

import EntitiesProducingBoundary._

object EntitiesPassThrough extends App {

  // def hylo[F[_]: Functor, A, B](a: A)(φ: Algebra[F, B], ψ: Coalgebra[F, A]): B = φ(ψ(a) ∘ (hylo(_)(φ, ψ)))

  import DbData._

  import FixPointTypes.rowFunctorImpl

  println(
    hylo[RowF, EntitiesInput, Json]((Seq(area), shiftsData, Nil, Nil, stopsData))(
      entitiesAlgebra,
      entitiesCoalgebraShort
    )
  )

}
