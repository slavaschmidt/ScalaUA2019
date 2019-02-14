package fpt.input

import java.time.ZonedDateTime
import java.util.UUID

object EntityBuilder {
  import scalikejdbc.WrappedResultSet

  def shiftFromRs(rs: WrappedResultSet): ShiftEntity = {
    val shift = ShiftEntity(
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
  def gradeFromRs(rs: WrappedResultSet): (UUID, GradeEntity) = {
    val shiftId = UUID.fromString(rs.string("shift_id"))
    val grade = GradeEntity(
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

  def orderFromRs(rs: WrappedResultSet): (UUID, UUID, OrderEntity) = {
    val shiftId = UUID.fromString(rs.string("shift_id"))
    val gradeId = UUID.fromString(rs.string("grade_id"))
    val order = OrderEntity(
      UUID.fromString(rs.string("id")),
      rs.get[ZonedDateTime]("time"),
      rs.double("average_speed"),
      rs.double("nominal_speed"),
      rs.double("width"),
      rs.double("order_length")
    )
    (shiftId, gradeId, order)
  }

  def stopFromRs(rs: WrappedResultSet): (UUID, UUID, UUID, StopEntity) = {
    val shiftId = UUID.fromString(rs.string("shift_id"))
    val gradeId = UUID.fromString(rs.string("grade_id"))
    val orderId = UUID.fromString(rs.string("order_id"))
    val stop = StopEntity(
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
