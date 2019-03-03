package fpt.input

import java.time.ZonedDateTime
import java.util.UUID

object DbData {
  val shiftIds: List[UUID] = List.fill(3)(UUID.randomUUID())
  val areaId = UUID.randomUUID()
  val area = AreaEntity(areaId)

  val shift1 = ShiftEntity(shiftIds(0), ZonedDateTime.now(), "first",  1, 1, 1, 1, 1, 1, 1)
  val shift2 = ShiftEntity(shiftIds(1), ZonedDateTime.now(), "second", 2, 2, 2, 2, 2, 2, 2)
  val shift3 = ShiftEntity(shiftIds(2), ZonedDateTime.now(), "third",  3, 3, 3, 3, 3, 3, 3)

  val shifts: Seq[ShiftEntity] = List(shift1, shift2, shift3)

  val shiftsData: Seq[(UUID, ShiftEntity)] = shifts.map { s =>
    (areaId, s)
  }

  val stop1 = StopEntity(UUID.randomUUID(), ZonedDateTime.now(), 1, true, "", "", "", "")
  val stop2 = StopEntity(UUID.randomUUID(), ZonedDateTime.now(), 1, true, "", "", "", "")
  val stop3 = StopEntity(UUID.randomUUID(), ZonedDateTime.now(), 2, true, "", "", "", "")

  val stops: Seq[StopEntity] = List(stop1, stop2, stop3,
    StopEntity(UUID.randomUUID(), ZonedDateTime.now(), 3, true, "", "", "", ""),
    StopEntity(UUID.randomUUID(), ZonedDateTime.now(), 4, true, "", "", "", ""),
    StopEntity(UUID.randomUUID(), ZonedDateTime.now(), 5, true, "", "", "", ""),
    StopEntity(UUID.randomUUID(), ZonedDateTime.now(), 6, true, "", "", "", ""),
    StopEntity(UUID.randomUUID(), ZonedDateTime.now(), 7, true, "", "", "", "")
  )

  val stopsData: Seq[(UUID, UUID, UUID, UUID, StopEntity)] = stops.map { s =>
    val id = shiftIds(scala.util.Random.nextInt(shiftIds.size))
    (areaId, id, id, id, s)
  }
}
