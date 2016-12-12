package org.shadowlands.tradetracker

import java.time.ZonedDateTime

import model._

package object processing {

  def accumEvents(prev: Traces, entries: List[Event], debug: Boolean = false, codes: Seq[String] = Nil): Traces = {

    def finish(unresolved: Map[Security, SecurityTrace], done: Traces) = {
      val updates = for {
        open <- unresolved
        res = done.getOrElse(open._1, Nil)
      } yield (open._1 -> (open._2 :: res))
      done ++ updates
    }

    def accum(open: Map[Security, SecurityTrace], done: Traces, left: List[Event]): Traces = left match {
      case Nil => finish(open, done)
      case ev :: rest if (open.contains(ev.security)) =>
        val trace = open(ev.security)
        if (debug || codes.contains(ev.security.asx_id)) System.out.println(s"*** ${ev.trade_date} Have security being added to existing trace - ${ev.security.asx_id} (holdings: ${trace.current.count}, ev type: ${ev.action.marker}, count: ${ev.units.count})")
        (open(ev.security) + ev) match {
          case (Some(matched), None) =>
            if (debug || codes.contains(ev.security.asx_id)) System.out.println(s"*** ${ev.trade_date} Finalised trace (${ev.security.asx_id}). Outcome: ${matched.net_outcome}.")
            accum(open - ev.security, done.updated(ev.security, matched :: done.getOrElse(ev.security, Nil)), rest)
          case (Some(matched), Some(remainder)) =>
            if (debug || codes.contains(ev.security.asx_id)) System.out.println(s"*** ${ev.trade_date} Finalised portion of trace - unmatched units: ${remainder.current.count}. Portion outcome: ${matched.net_outcome}.")
            accum(open.updated(ev.security, remainder), done.updated(ev.security, matched :: done.getOrElse(ev.security, Nil)), rest)
          case (_, Some(trace)) if trace.finalised && done.contains(ev.security) =>
            if (debug || codes.contains(ev.security.asx_id)) System.out.println(s"*** ${ev.trade_date} Finalised trace - units left: ${trace.current.count}. Outcome: ${trace.net_outcome}. There are other (finalised) traces for this security")
            accum(open - ev.security, done.updated(ev.security, trace :: done(ev.security)), rest)
          case (_, Some(trace)) if trace.finalised =>
            if (debug || codes.contains(ev.security.asx_id)) System.out.println(s"*** ${ev.trade_date} Finalised trace - units left: ${trace.current.count}. Outcome: ${trace.net_outcome}. First (finalised) trace for this security")
            accum(open - ev.security, done.updated(ev.security, trace :: Nil), rest)
          case (_, Some(trace)) =>
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
    }

    val done = prev.mapValues(traces => traces.filter(_.finalised))
    val open = prev.mapValues(traces => traces.find(!_.finalised)).filter(_._2.isDefined).mapValues(_.get)
    accum(open, done, entries)
  }

}
