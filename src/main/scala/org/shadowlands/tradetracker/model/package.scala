package org.shadowlands.tradetracker

import java.time.LocalDate

package object model {

  case class Security(asx_id: String)

  sealed class Action(val str: String, val marker: Char, val sign: Int)
  object Action {
    case object Buy extends Action("Buy", 'B', 1)
    case object Sell extends Action("Sell", 'S', -1)
    case object NameChange extends Action("Rename", 'N', -1)
    case object Consolidation extends Action("Consolidation", 'C', -1)
    case object OptionExercise extends Action("Options Exercised", 'O', -1)
    case object OptionsLapsed extends Action("Options Lapsed", 'L', -1)
    case object Purchase extends Action("Purchase", 'P', 1)

    def fromStr(str: String): Action = str match {
      case "B" | "Buy" => Action.Buy
      case "S" | "Sell" => Action.Sell
      case "N" | "Rename" => Action.NameChange
      case "C" | "Consolidation" => Action.Consolidation
      case "O" | "Options Exercised" => Action.OptionExercise
      case "L" | "Options Lapsed" => Action.OptionsLapsed
      case "P" | "Purchase" => Action.Purchase
      case other => throw new IllegalArgumentException(s"Unknown action: $other")
    }
  }

  sealed case class Currency(name: String, symbol: Char, decimals: Int)
  object Currency {
    val AU = Currency("AUST $", '$', 2)
  }
  case class Money(amount: Double = 0.0, unit: Currency = Currency.AU) {
    def +(amt: Money) = copy(amount = amount + amt.amount)
    def -(amt: Money) = copy(amount = amount - amt.amount)
    def *(amt: Int) = copy(amount = amt * amount)

    override def toString() = s"%s%.${unit.decimals}f".format(unit.symbol, amount)
  }
  object Money {
    val NoAmount = Money()
  }

  case class Price(amount: Double, unit: Currency = Currency.AU, decimals: Int = 3) {
    override def toString() = s"%s%.${decimals}f".format(unit.symbol, amount)
  }
  object Price {
    val NoPrice = Price(0.0)
  }

  case class Confirmation(number: Int)

  case class Order(number: String)

  case class Units(count: Int) {
    def +(change: Int) = copy(count + change)
    def -(change: Int) = copy(count - change)
    def *(amt: Int) = copy(count = amt * count)
    def isZero = count == 0
  }

  case class Event(confirmation: Confirmation, order: Order, trade_date: LocalDate, action: Action,
                   security: Security, units: Units, ave_price: Price, brokerage: Money, net_proc: Money,
                   settle_date: LocalDate, alt_name: Option[Security] = None, alt_units: Option[Units] = None)
      extends Ordered[Event] {

    override def compare(that: Event): Int = trade_date.compareTo(that.trade_date)

    def flip(net_outcome: Money, costs: Money) = alt_name match {
      case Some(alt_sec) => copy(security = alt_sec, units = units * -1, brokerage = costs,
                                 net_proc = net_outcome + this.net_proc * this.action.sign, alt_name = None)
      case _ => this
    }
  }

  case class SecurityTrace(security: Security, events: List[Event], current: Units, finalised: Boolean,
                           net_outcome: Money, costs: Money, start: LocalDate, end: LocalDate)

  object SecurityTrace {
    def apply(init: Event): SecurityTrace = SecurityTrace(init.security, init :: Nil,
        init.units.copy(count = init.units.count * init.action.sign), false, init.net_proc * -init.action.sign,
        init.brokerage, init.trade_date, init.trade_date)
    def apply(init: Event, second: Event): SecurityTrace = {
      val count = init.units.count * init.action.sign + second.units.count * second.action.sign
      SecurityTrace(init.security, second :: init :: Nil,
                    init.units.copy(count = count), count == 0,
                    init.net_proc * -init.action.sign + second.net_proc * -second.action.sign,
                    init.brokerage + second.brokerage, init.trade_date, second.trade_date)
    }
  }

  type Traces = Map[Security, List[SecurityTrace]]

}
