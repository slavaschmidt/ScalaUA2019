package fpt.input

import java.time.ZonedDateTime
import java.util.UUID

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
