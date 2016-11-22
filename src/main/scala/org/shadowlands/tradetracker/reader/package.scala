package org.shadowlands.tradetracker

import java.nio.file.Path
import java.text.SimpleDateFormat
import java.util.Date

import purecsv.unsafe._

import model._

package object reader {

  case class Template(confirmation: Confirmation, order: Order, trade_date: Date, action: Action,
                      security: Security, units: Units, ave_price: Price, brokerage: Money, net_proc: Money,
                      settle_date: Date, conf: String)

  object Converters {

    import purecsv.unsafe.converter.StringConverter

    implicit val date = new StringConverter[Date] {
      override def from(str: String): Date = new SimpleDateFormat("dd/MM/yyyy").parse(str)
      override def to(date: Date): String = date.toString
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
      override def from(str: String): Units = Units(str.toInt)
      override def to(units: Units): String = units.count.toString
    }
    implicit val price = new StringConverter[Price] {
      override def from(str: String): Price = Price(str.toDouble)
      override def to(price: Price): String = price.amount.toString
    }
    implicit val money = new StringConverter[Money] {
      override def from(str: String): Money = Money(str.toDouble)
      override def to(money: Money): String = money.amount.toString
    }
  }

  def readCsv(path: Path): Either[String, List[Template]] = try {
    import Converters._
    val lines = CSVReader[Template].readCSVFromFile(path.toFile, true)
    Right(lines.sortBy(_.trade_date))
  } catch {
    case ex: Exception =>
      ex.printStackTrace()
      Left(ex.getLocalizedMessage)
  }

  def toEvent(entry: Template) = Event(entry.confirmation, entry.order, entry.trade_date, entry.action, entry.security,
                                       entry.units, entry.ave_price, entry.brokerage, entry.net_proc, entry.settle_date)

}

