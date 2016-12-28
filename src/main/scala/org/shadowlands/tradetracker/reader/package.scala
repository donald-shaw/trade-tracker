package org.shadowlands.tradetracker

import java.nio.file.Path
import java.time.LocalDate
import java.time.format.DateTimeFormatter

import purecsv.unsafe._

import model._

package object reader {

  sealed abstract class Template(val confirmation: Confirmation, val order: Order, val trade_date: LocalDate,
                                 val action: Action, val security: Security, val units: Units, val ave_price: Price,
                                 val brokerage: Money, val net_proc: Money, val settle_date: LocalDate,
                                 val alt_security: Option[Security] = None, val alt_units: Option[Units] = None)
  sealed trait TemplateType[T <: Template] {
    type template = T
  }
  object TemplateType {
    case object CS extends TemplateType[CS_Template]
    case object NW extends TemplateType[NW_Template]
    case object other extends TemplateType[other_Template]

    def fromString(t_type: String): TemplateType[_ <: Template] = t_type match {
      case "CS" => CS
      case "NW" => NW
      case "other" => other
    }
  }

  case class CS_Template(override val confirmation: Confirmation, override val order: Order, override val trade_date: LocalDate,
                         override val action: Action, override val security: Security, override val units: Units,
                         override val ave_price: Price, override val brokerage: Money, override val net_proc: Money,
                         override val settle_date: LocalDate, conf: String)
    extends Template(confirmation, order, trade_date, action, security, units, ave_price, brokerage, net_proc, settle_date)

  case class NW_Template(override val trade_date: LocalDate, override val confirmation: Confirmation, override val order: Order,
                         override val action: Action, override val security: Security, override val units: Units,
                         override val ave_price: Price, override val brokerage: Money, override val net_proc: Money,
                         override val settle_date: LocalDate, conf: String)
    extends Template(confirmation, order, trade_date, action, security, units, ave_price, brokerage, net_proc, settle_date)

  case class other_Template(override val confirmation: Confirmation, override val order: Order, date: LocalDate,
                            override val action: Action, override val security: Security, override val units: Units,
                            ave_price_opt: Option[Price], brokerage_opt: Option[Money], net_proc_opt: Option[Money],
                            override val alt_security: Option[Security], override val alt_units: Option[Units])
    extends Template(confirmation, order, date, action, security, units, ave_price_opt.getOrElse(Price.NoPrice),
                     brokerage_opt.getOrElse(Money.NoAmount), net_proc_opt.getOrElse(Money.NoAmount), date, alt_security, alt_units)

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

  def readCsv[T <: Template](path: Path, templ_type: TemplateType[T]): Either[String, List[Template]] = try {
    import Converters._
//    val lines = CSVReader[NW_Template].readCSVFromFile(path.toFile, true)
    val lines = templ_type match {
      case TemplateType.CS => CSVReader[CS_Template].readCSVFromFile(path.toFile, true)
      case TemplateType.NW => CSVReader[NW_Template].readCSVFromFile(path.toFile, true)
      case TemplateType.other => CSVReader[other_Template].readCSVFromFile(path.toFile, true)
    }
    //val lines = CSVReader[templ_type.template].readCSVFromFile(path.toFile, true)
    Right(lines.reverse)
  } catch {
    case ex: Exception =>
      ex.printStackTrace()
      Left(ex.getLocalizedMessage)
  }

  def toEvent(entry: Template) = Event(entry.confirmation, entry.order, entry.trade_date, entry.action, entry.security,
                                       entry.units, entry.ave_price, entry.brokerage, entry.net_proc, entry.settle_date,
                                       entry.alt_security, entry.alt_units)

//  def toEvent(entry: NW_Template) = Event(entry.confirmation, entry.order, entry.trade_date, entry.action, entry.security,
//    entry.units, entry.ave_price, entry.brokerage, entry.net_proc, entry.settle_date)
//
//  def toEvent(entry: other_Template) = Event(entry.confirmation, entry.order, entry.date, entry.action, entry.security,
//    entry.units, entry.ave_price.getOrElse(Price.NoPrice), entry.brokerage.getOrElse(Money.NoAmount),
//    entry.net_proc.getOrElse(Money.NoAmount), entry.date, entry.alt_security, entry.alt_units)

}
