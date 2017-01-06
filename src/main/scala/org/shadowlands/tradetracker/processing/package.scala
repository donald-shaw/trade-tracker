package org.shadowlands.tradetracker

import java.time.ZonedDateTime

import model._
import model.Action._

package object processing {

  def accumEvents(prev: Traces, entries: List[Event], debug: Boolean = false, codes: Seq[String] = Nil): Traces = {

    def finish(unresolved: Map[Security, SecurityTrace], done: Traces) = {
      val updates = for {
        open <- unresolved
        res = done.getOrElse(open._1, Nil)
      } yield (open._1 -> (open._2 :: res))
      done ++ updates
    }

    def add(trace: SecurityTrace, event: Event) = {
      val upd_units = trace.current + event.units.count * event.action.sign
      val upd_outcome = trace.net_outcome - event.net_proc * event.action.sign
      trace.copy(events = event :: trace.events, current = upd_units, finalised = upd_units.isZero,
                 net_outcome = upd_outcome, costs = trace.costs + event.brokerage, end = event.trade_date)
    }

    def combine(trace: SecurityTrace, event: Event) = event.action match {
      case Sell | OptionsLapsed => trace.events.filter(ev => ev.action == Action.Buy && !ev.trade_date.isAfter(event.trade_date))
                               .find(_.units == event.units) match {
        case Some(buy) =>
          val upd_units = trace.current - buy.units.count * buy.action.sign
          val upd_outcome = trace.net_outcome + buy.net_proc * buy.action.sign
          val matched = SecurityTrace(buy, event)
          if (trace.events.size > 1) {
            (Some(matched),
             Some(trace.copy(events = trace.events.filterNot(_ == buy), current = upd_units, finalised = upd_units.isZero,
                  net_outcome = upd_outcome, costs = trace.costs + event.brokerage, end = event.trade_date)),
             None)
          } else (Some(matched), None, None)
        case _ => (None, Some(add(trace, event)), None)
      }

      case Buy | Purchase => (None, Some(add(trace, event)), None)

      case NameChange | OptionExercise =>
        trace.events.filter(ev => ev.action == Action.Sell && !ev.trade_date.isBefore(event.trade_date))
                                    .find(_.units.count == -event.units.count) match {
          case Some(sell) =>
            if (debug) println(s"@@@ Have matched sell for re-name/option-ex: $sell - (flipped) re-name/option-ex: $event - trace: $trace")
            val upd_units = trace.current - event.units.count
            val upd_outcome = trace.net_outcome - event.net_proc * event.action.sign
            val upd_costs = trace.costs + event.brokerage
            val upd_trace = trace.copy(events = event :: trace.events, current = upd_units, finalised = upd_units.isZero,
                                       net_outcome = upd_outcome, costs = upd_costs, start = event.trade_date)
            (None, Some(upd_trace), None)
          case _ =>
            if (debug) println(s"@@@ Standard match for re-name/option-ex: $event - trace: $trace")
            val upd_units = trace.current - event.units.count
            val upd_trace = trace.copy(events = event :: trace.events, current = upd_units, finalised = upd_units.isZero,
            net_outcome = Money.NoAmount, costs = Money.NoAmount, end = event.trade_date)
            (None, Some(upd_trace), Some(event.flip(trace.net_outcome, trace.costs)))
        }

      case Consolidation =>
        val upd_units = trace.current - event.units.count + event.alt_units.map(_.count).getOrElse(0)
        val upd_trace = trace.copy(events = event :: trace.events, current = upd_units, finalised = upd_units.isZero,
          end = if (event.trade_date.isAfter(trace.end)) event.trade_date else trace.end)
        (None, Some(upd_trace), None)
    }

    def accum(open: Map[Security, SecurityTrace], done: Traces, left: List[Event]): Traces = {
        if (debug) println(s"### re-entered accum -\nleft:\n${left.mkString(",\n")}\n\nopen:\n${open.mkString(",\n")}\n###\n")
        left match {
      case Nil => finish(open, done)
      case ev :: rest if (open.contains(ev.security)) =>
        val trace = open(ev.security)
        if (debug || codes.contains(ev.security.asx_id)) System.out.println(s"*** ${ev.trade_date} Have security being added to existing trace - ${ev.security.asx_id} (holdings: ${trace.current.count}, ev type: ${ev.action.marker}, count: ${ev.units.count})")
        combine(open(ev.security), ev) match {
          case (Some(matched), None, _) =>
            if (debug || codes.contains(ev.security.asx_id)) System.out.println(s"*** ${ev.trade_date} Finalised trace (${ev.security.asx_id}). Outcome: ${matched.net_outcome}.")
            accum(open - ev.security, done.updated(ev.security, matched :: done.getOrElse(ev.security, Nil)), rest)
          case (Some(matched), Some(remainder), flipped_event) =>
            if (debug || codes.contains(ev.security.asx_id)) System.out.println(s"*** ${ev.trade_date} Finalised portion of trace - unmatched units: ${remainder.current.count}. Portion outcome: ${matched.net_outcome}. Flipped event: $flipped_event")
            accum(open.updated(ev.security, remainder), done.updated(matched.security, matched :: done.getOrElse(matched.security, Nil)), flipped_event.map(_ :: rest).getOrElse(rest))
          case (_, Some(trace), _) if trace.finalised && done.contains(ev.security) =>
            if (debug || codes.contains(ev.security.asx_id)) System.out.println(s"*** ${ev.trade_date} Finalised trace - units left: ${trace.current.count}. Outcome: ${trace.net_outcome}. There are other (finalised) traces for this security")
            accum(open - ev.security, done.updated(ev.security, trace :: done(ev.security)), rest)
          case (_, Some(trace), flipped_event) if trace.finalised =>
            if (debug || codes.contains(ev.security.asx_id)) System.out.println(s"*** ${ev.trade_date} Finalised trace - units left: ${trace.current.count}. Outcome: ${trace.net_outcome}. First (finalised) trace for this security. Flipped event: $flipped_event")
            accum(open - ev.security, done.updated(ev.security, trace :: Nil), flipped_event.map(_ :: rest).getOrElse(rest))
          case (_, Some(trace), _) =>
            if (debug || codes.contains(ev.security.asx_id)) System.out.println(s"*** ${ev.trade_date} Updated (unfinalised) trace - units left now: ${trace.current.count}")
            accum(open.updated(ev.security, trace), done, rest)
        }
      case ev :: rest =>
        if (debug || codes.contains(ev.security.asx_id)) System.out.println(s"*** ${ev.trade_date} Have security starting new trace - ${ev.security.asx_id} - ev type: ${ev.action.marker}, count: ${ev.units.count}")
        val trace = SecurityTrace(ev)
        if (trace.finalised) {
          if (debug || codes.contains(ev.security.asx_id)) System.out.println(s"*** ${ev.trade_date} Finalised (new) trace - units 'left': ${trace.current.count}. Outcome: ${trace.net_outcome}")
          accum(open, done.updated(ev.security, trace :: done.getOrElse(ev.security, Nil)), rest)
        } else {
          accum(open.updated(ev.security, trace), done, rest)
        }
    } }

    val done = prev.mapValues(traces => traces.filter(_.finalised)).filterNot(entry => entry._2.isEmpty)
    val open = prev.mapValues(traces => traces.find(!_.finalised)).filter(_._2.isDefined).mapValues(_.get)
    if (debug) println(s"\n\n***\n'Done' traces: \n${done.mkString(",\n")}\n\n'Open' traces: \n${open.mkString(",\n")}\n***\n\n")
    accum(open, done, entries)
  }

}
