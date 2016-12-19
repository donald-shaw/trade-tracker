package org.shadowlands.tradetracker.model.json

import java.time.LocalDate
import java.time.format.DateTimeFormatter

import org.shadowlands.tradetracker.model._
import org.shadowlands.tradetracker.model.json.JsonEntities.{JsonEvent, JsonTrace}

object JsonConversions {

  val date_formatter = DateTimeFormatter.ISO_DATE

  def toDateStr(dt: LocalDate) = dt.format(date_formatter)
  def toLocalDate(dt_str: String) = LocalDate.parse(dt_str, date_formatter)

  def toJsonEvent(event: Event) =
    JsonEvent(event.confirmation.number, event.order.number, toDateStr(event.trade_date), event.action.str,
              event.units.count, event.security.asx_id, event.ave_price.unit.name, event.ave_price.amount,
              event.brokerage.amount, event.net_proc.amount, toDateStr(event.settle_date))

  def fromJsonEvent(event: JsonEvent) = {
    val curr = Currency.AU
    Event(Confirmation(event.conf_num), Order(event.order_num), toLocalDate(event.trade_date),
          Action.fromStr(event.action), Security(event.security), Units(event.units), Price(event.ave_price, curr),
          Money(event.brokerage, curr), Money(event.net_proceeds, curr), toLocalDate(event.settle_date))
  }

  def toJsonTrace(trace: SecurityTrace) =
    JsonTrace(trace.security.asx_id, trace.events.map(toJsonEvent), trace.current.count, trace.finalised,
              trace.net_outcome.unit.name, trace.net_outcome.amount, trace.costs.amount, toDateStr(trace.start),
              toDateStr(trace.end))

  def fromJsonTrace(trace: JsonTrace): SecurityTrace = {
    val curr = Currency.AU
    SecurityTrace(Security(trace.security), trace.events.map(fromJsonEvent), Units(trace.current), trace.finalised,
                  Money(trace.net_outcome, curr), Money(trace.costs, curr), toLocalDate(trace.start),
                  toLocalDate(trace.end))
  }
}
