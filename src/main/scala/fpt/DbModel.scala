package fpt

import java.time.ZonedDateTime
import java.util.UUID

import scala.language.higherKinds
import scalaz._
import matryoshka._
import matryoshka.data.Fix
import matryoshka.implicits._

sealed trait RowF[+A]
final case class ParentRowF[E <: Entity, B](row: E, children: Seq[B]) extends RowF[B]
final case class BottomRowF[E <: Entity, B](row: E) extends RowF[B]

// final case class AreaF[A](id: UUID, children: Seq[A])
// final case class ShiftF[A](shift: ShiftEntity, children: Seq[A]) extends RowF[+A]
// final case class GradeF[A](grade: GradeEntity, children: Seq[A]) extends RowF[+A]
// final case class OrderF[A](shift: OrderEntity, children: Seq[A]) extends RowF[+A]
// final case class StopF[A](stop: StopEntity,    children: Seq[A]) extends RowF[+A]

object FixPointTypes extends App {

  implicit val rowFunctorImpl: Functor[RowF] = new Functor[RowF] {
    override def map[A, B](a: RowF[A])(f: A => B): RowF[B] = a match {
      case ParentRowF(r, d) => ParentRowF(r, d.map(f))
      case BottomRowF(r) => BottomRowF(r)
    }
  }

  implicit val rowsFunctorImpl: Functor[RowsF] = new Functor[RowsF] {
    override def map[A, B](a: RowsF[A])(f: A => B): RowsF[B] = {
      val F = implicitly[Functor[RowF]]
      a.map { rowF =>
        F.map(rowF)(f)
      }
    }
  }

  type RowsF[A] = List[RowF[A]]

  type EntitiesInput = (
    List[ShiftEntity],
      List[(UUID, GradeEntity)],
      List[(UUID, UUID, OrderEntity)],
      List[(UUID, UUID, UUID, StopEntity)]
    )

  val shiftStopsCoalgebra: Coalgebra[RowsF, EntitiesInput] = {
    case (Nil, Nil, Nil, stops) =>
      stops.map(_._4).map(BottomRowF.apply)
    case (shifts, Nil, orders, stops) =>
      shifts.map { s =>
        ParentRowF(s, List((Nil, Nil, orders, stops.filter(_._1 == s.id))))
      }
    case others => ??? // TODO somehow it feels wrong
  }

  def toShiftStops(shifts: List[ShiftEntity], stops: List[(UUID, UUID, UUID, StopEntity)]) =
    (shifts, List.empty[(UUID, GradeEntity)], List.empty[(UUID, UUID, OrderEntity)], stops)
      .ana[Fix[RowsF]](shiftStopsCoalgebra)

  val shiftIds = List.fill(3)(UUID.randomUUID())

  val shifts = List(
    ShiftEntity(shiftIds(0), ZonedDateTime.now(), "first",  1, 1, 1, 1, 1, 1, 1),
    ShiftEntity(shiftIds(1), ZonedDateTime.now(), "second", 2, 2, 2, 2, 2, 2, 2),
    ShiftEntity(shiftIds(2), ZonedDateTime.now(), "third",  3, 3, 3, 3, 3, 3, 3)
  )

  val stops = List(
    StopEntity(UUID.randomUUID(), ZonedDateTime.now(), 1, true, "", "", "", ""),
    StopEntity(UUID.randomUUID(), ZonedDateTime.now(), 2, true, "", "", "", ""),
    StopEntity(UUID.randomUUID(), ZonedDateTime.now(), 3, true, "", "", "", ""),
    StopEntity(UUID.randomUUID(), ZonedDateTime.now(), 4, true, "", "", "", ""),
    StopEntity(UUID.randomUUID(), ZonedDateTime.now(), 5, true, "", "", "", ""),
    StopEntity(UUID.randomUUID(), ZonedDateTime.now(), 6, true, "", "", "", ""),
    StopEntity(UUID.randomUUID(), ZonedDateTime.now(), 7, true, "", "", "", "")
  )

  val stopsData = stops.map { s =>
    val id = shiftIds(scala.util.Random.nextInt(shiftIds.size))
    (id, id, id, s)
  }
  println(toShiftStops(shifts, stopsData))
}

sealed trait Entity {
  def id: UUID
  def time: ZonedDateTime
}

final case class AreaEntity(id: UUID) extends Entity {
  override def time: ZonedDateTime = ZonedDateTime.now() // TODO this does not make sense
}

final case class ShiftEntity(
                              id: UUID,
                              time: ZonedDateTime,
                              name: String,
                              duration: Long,
                              breakDuration: Long,
                              shiftLm: Double,
                              shiftSm: Double,
                              averageSpeed: Double,
                              numberOfOrderChanges: Long,
                              numberOfGradeChanges: Long
                            ) extends Entity

final case class GradeEntity(
                              id: UUID,
                              time: ZonedDateTime,
                              fluteId: String,
                              lm: Double,
                              sm: Double,
                              asmSm: Double,
                              hqmSm: Double,
                              kqfSm: Double,
                              kqmSm: Double,
                              sideTrimSm: Double,
                              weight: Option[Long]
                            ) extends Entity

final case class OrderEntity(
                              id: UUID,
                              time: ZonedDateTime,
                              averageSpeed: Double,
                              nominalSpeed: Double,
                              width: Double,
                              length: Double
                            ) extends Entity

final case class StopEntity(
                             id: UUID,
                             time: ZonedDateTime,
                             stopDuration: Long,
                             isBhsStop: Boolean,
                             machineGroup: String,
                             machineName: String,
                             stopReason: String,
                             stopText: String
                           ) extends Entity

object EntityBuilder {
  import scalikejdbc.WrappedResultSet

  def shiftFromRs(rs: WrappedResultSet) = {
    val shift = new ShiftEntity(
      UUID.fromString(rs.get[String]("id")),
      rs.get[ZonedDateTime]("time"),
      rs.string("name"),
      rs.long("duration"),
      rs.long("break_duration"),
      rs.double("lineal_meter"),
      rs.double("square_meter"),
      rs.double("average_speed"),
      rs.long("number_of_order_changes"),
      rs.long("number_of_grade_changes")
    )
    shift
  }
  def gradeFromRs(rs: WrappedResultSet) = {
    val shiftId = UUID.fromString(rs.string("shift_id"))
    val grade = new GradeEntity(
      UUID.fromString(rs.string("id")),
      rs.get[ZonedDateTime]("time"),
      rs.string("flute_identifier"),
      rs.double("lineal_meter"),
      rs.double("square_meter"),
      rs.double("square_meter_wasteasm"),
      rs.double("square_meter_wastehqm"),
      rs.double("square_meter_wastekqf"),
      rs.double("square_meter_wastekqm"),
      rs.double("square_meter_side_trim"),
      rs.longOpt("grade_weight")
    )
    (shiftId, grade)
  }

  def orderFromRs(rs: WrappedResultSet) = {
    val shiftId = UUID.fromString(rs.string("shift_id"))
    val gradeId = UUID.fromString(rs.string("grade_id"))
    val order = new OrderEntity(
      UUID.fromString(rs.string("id")),
      rs.get[ZonedDateTime]("time"),
      rs.double("average_speed"),
      rs.double("nominal_speed"),
      rs.double("width"),
      rs.double("order_length")
    )
    (shiftId, gradeId, order)
  }

  def stopFromRs(rs: WrappedResultSet) = {
    val shiftId = UUID.fromString(rs.string("shift_id"))
    val gradeId = UUID.fromString(rs.string("grade_id"))
    val orderId = UUID.fromString(rs.string("order_id"))
    val stop = new StopEntity(
      UUID.fromString(rs.string("id")),
      rs.get[ZonedDateTime]("time"),
      rs.long("duration"),
      rs.boolean("bhs_stop"),
      rs.string("machine_group_name"),
      rs.string("machine_name"),
      rs.string("stop_reason"),
      rs.string("stop_text")
    )
    (shiftId, gradeId, orderId, stop)
  }

}
