package fpt.input

import java.time.ZonedDateTime
import java.util.UUID

object DbData {
  val shiftIds: List[UUID] = List.fill(3)(UUID.randomUUID())

  val area = AreaEntity(UUID.randomUUID())

  val shifts: Seq[ShiftEntity] = List(
    ShiftEntity(shiftIds(0), ZonedDateTime.now(), "first",  1, 1, 1, 1, 1, 1, 1),
    ShiftEntity(shiftIds(1), ZonedDateTime.now(), "second", 2, 2, 2, 2, 2, 2, 2),
    ShiftEntity(shiftIds(2), ZonedDateTime.now(), "third",  3, 3, 3, 3, 3, 3, 3)
  )

  val stops: Seq[StopEntity] = List(
    StopEntity(UUID.randomUUID(), ZonedDateTime.now(), 1, true, "", "", "", ""),
    StopEntity(UUID.randomUUID(), ZonedDateTime.now(), 2, true, "", "", "", ""),
    StopEntity(UUID.randomUUID(), ZonedDateTime.now(), 3, true, "", "", "", ""),
    StopEntity(UUID.randomUUID(), ZonedDateTime.now(), 4, true, "", "", "", ""),
    StopEntity(UUID.randomUUID(), ZonedDateTime.now(), 5, true, "", "", "", ""),
    StopEntity(UUID.randomUUID(), ZonedDateTime.now(), 6, true, "", "", "", ""),
    StopEntity(UUID.randomUUID(), ZonedDateTime.now(), 7, true, "", "", "", "")
  )

  val stopsData: Seq[(UUID, UUID, UUID, StopEntity)] = stops.map { s =>
    val id = shiftIds(scala.util.Random.nextInt(shiftIds.size))
    (id, id, id, s)
  }
}
