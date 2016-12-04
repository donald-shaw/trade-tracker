package org.shadowlands.tradetracker

import java.time.LocalDate

package object model {

  case class Security(asx_id: String)

  sealed class Action(val str: String, val marker: Char, val sign: Int)
  object Action {
    case object Buy extends Action("Buy", 'B', 1)
    case object Sell extends Action("Sell", 'S', -1)

    def fromStr(str: String): Action = str match {
      case "B" => Action.Buy
      case "S" => Action.Sell
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

  case class Price(amount: Double, unit: Currency = Currency.AU, decimals: Int = 3) {
    override def toString() = s"%s%.${decimals}f".format(unit.symbol, amount)
  }

  case class Confirmation(number: Int)

  case class Order(number: String)

  case class Units(count: Int) {
    def +(change: Int) = copy(count + change)
    def -(change: Int) = copy(count - change)
    def isZero = count == 0
  }

  case class Event(confirmation: Confirmation, order: Order, trade_date: LocalDate, action: Action,
                   security: Security, units: Units, ave_price: Price, brokerage: Money, net_proc: Money,
                   settle_date: LocalDate)

  case class SecurityTrace(security: Security, events: List[Event], current: Units, resolved: Boolean,
                           net_outcome: Money, costs: Money, start: LocalDate, end: LocalDate, unresolvable: Boolean = false) {
    def +(ev: Event) = {
      val upd_units = current + ev.units.count * ev.action.sign
      val upd_outcome = net_outcome - ev.net_proc * ev.action.sign
      copy(events = ev :: events, current = upd_units, resolved = upd_units.isZero && !unresolvable,
           net_outcome = upd_outcome, costs = costs + ev.brokerage, end = ev.trade_date)
    }
  }
  object SecurityTrace {
    def apply(init: Event): SecurityTrace = SecurityTrace(init.security, init :: Nil,
        init.units.copy(count = init.units.count * init.action.sign), false, init.net_proc * -init.action.sign,
        init.brokerage, init.trade_date, init.trade_date, init.action == Action.Sell)
  }

  type Traces = Map[Security, List[SecurityTrace]]

}
