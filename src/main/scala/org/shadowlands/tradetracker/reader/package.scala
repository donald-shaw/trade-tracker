package org.shadowlands.tradetracker

import java.nio.file.Path
import java.time.LocalDate
import java.time.format.DateTimeFormatter

import purecsv.unsafe._

import model._

package object reader {

  case class CS_Template(confirmation: Confirmation, order: Order, trade_date: LocalDate, action: Action,
                      security: Security, units: Units, ave_price: Price, brokerage: Money, net_proc: Money,
                      settle_date: LocalDate, conf: String)

  case class NW_Template(trade_date: LocalDate, confirmation: Confirmation, order: Order, action: Action,
                      security: Security, units: Units, ave_price: Price, brokerage: Money, net_proc: Money,
                      settle_date: LocalDate, conf: String, blank: String)

  object Converters {

    import purecsv.unsafe.converter.StringConverter

    def numStr(str: String) = str.filterNot(Set('"', ',', '$').contains).trim

    implicit val date = new StringConverter[LocalDate] {
      override def from(str: String): LocalDate = LocalDate.parse(str, DateTimeFormatter.ofPattern("d/M/yyyy"))
      override def to(date: LocalDate): String = date.toString
    }
    implicit val confirmation = new StringConverter[Confirmation] {
      override def from(str: String): Confirmation = Confirmation(str.toInt)
      override def to(conf: Confirmation): String = conf.number.toString
    }
    implicit val order = new StringConverter[Order] {
      override def from(str: String): Order = Order(str)
      override def to(order: Order): String = order.number
    }
    implicit val action = new StringConverter[Action] {
      override def from(str: String): Action = Action.fromStr(str)
      override def to(action: Action): String = action.marker.toString
    }
    implicit val security = new StringConverter[Security] {
      override def from(str: String): Security = Security(str)
      override def to(security: Security): String = security.asx_id
    }
    implicit val units = new StringConverter[Units] {
      override def from(str: String): Units = Units(numStr(str).toInt)
      override def to(units: Units): String = units.count.toString
    }
    implicit val price = new StringConverter[Price] {
      override def from(str: String): Price = Price(numStr(str).toDouble)
      override def to(price: Price): String = price.amount.toString
    }
    implicit val money = new StringConverter[Money] {
      override def from(str: String): Money = Money(numStr(str).toDouble)
      override def to(money: Money): String = money.amount.toString
    }
  }

  def readCsv(path: Path): Either[String, List[NW_Template]] = try {
    import Converters._
    val lines = CSVReader[NW_Template].readCSVFromFile(path.toFile, true)
    Right(lines.reverse)
  } catch {
    case ex: Exception =>
      ex.printStackTrace()
      Left(ex.getLocalizedMessage)
  }

  def toEvent(entry: NW_Template) = Event(entry.confirmation, entry.order, entry.trade_date, entry.action, entry.security,
                                       entry.units, entry.ave_price, entry.brokerage, entry.net_proc, entry.settle_date)

}
