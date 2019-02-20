package fpt.calculation

import java.time.LocalDate

import fpt.input._
import scalaz.{Cofree, Functor}
import matryoshka._
import matryoshka.implicits._
import matryoshka.patterns._
import matryoshka.data._
import java.util._
import java.time._
import java.time.format._
import java.time.chrono._
import java.time.temporal._

import scala.language.higherKinds

import enumeratum._

sealed trait TimeAlignment extends EnumEntry {
  def apply(d: ZonedDateTime): LocalDate
}

object TimeAlignment extends Enum[TimeAlignment] with CirceEnum[TimeAlignment] {
  override val values = findValues
  final case object Day extends TimeAlignment {
    override def apply(d: ZonedDateTime): LocalDate = d.truncatedTo(ChronoUnit.DAYS).toLocalDate
  }
  final case object Week extends TimeAlignment {
    override def apply(d: ZonedDateTime): LocalDate = d.truncatedTo(ChronoUnit.HOURS).toLocalDate
  }
  // final case object Month extends TimeAlignment {
  //   override def apply(d: ZonedDateTime): LocalDate = d.truncatedTo(ChronoUnit.MONTHS).toLocalDate
  // }
  // final case object Year extends TimeAlignment {
  //   override def apply(d: ZonedDateTime): LocalDate = d.truncatedTo(ChronoUnit.YEARS).toLocalDate
  // }
}
