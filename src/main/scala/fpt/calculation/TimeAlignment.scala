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
  def apply(d: ZonedDateTime): Int
}

object TimeAlignment extends Enum[TimeAlignment] with CirceEnum[TimeAlignment] {
  override val values = findValues
  final case object Day extends TimeAlignment {
    override def apply(d: ZonedDateTime): Int = d.getDayOfYear + d.getYear * 1000
  }
  final case object Week extends TimeAlignment {
    override def apply(d: ZonedDateTime): Int = {
      val year       = d.get(IsoFields.WEEK_BASED_YEAR)
      val weekOfYear = d.get(IsoFields.WEEK_OF_WEEK_BASED_YEAR)
      weekOfYear + year * 100
    }
  }
  final case object Month extends TimeAlignment {
    override def apply(d: ZonedDateTime): Int = d.getMonth.getValue + (d.getYear * 100)
  }
  final case object Year extends TimeAlignment {
    override def apply(d: ZonedDateTime): Int = d.getYear
  }
}
