package org.shadowlands.tradetracker

import model._

package object processing {

  def accumEvents(prev: Traces, entries: List[Event]): Traces = {

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
        System.out.println(s"*** Have security being added to existing trace - ${ev.security.asx_id} (holdings: ${trace.current.count}, ev type: ${ev.action.marker}, count: ${ev.units.count})")
        (open(ev.security) + ev) match {
        case trace if trace.resolved && done.contains(ev.security) =>
          System.out.println(s"*** Finalised trace - units left: ${trace.current.count}. There are other (finalised) traces for this security")
          accum(open - ev.security, done.updated(ev.security, trace :: done(ev.security)), rest)
        case trace if trace.resolved =>
          System.out.println(s"*** Finalised trace - units left: ${trace.current.count}. First (finalised) trace for this security")
          accum(open - ev.security, done.updated(ev.security, trace :: Nil), rest)
        case trace =>
          System.out.println(s"*** Updated (unfinalised) trace - units left now: ${trace.current.count} ${if (trace.unresolvable) "(NB: not resolvable)" else ""}")
          accum(open.updated(ev.security, trace), done, rest)
      }
      case ev :: rest =>
        System.out.println(s"*** Have security starting new trace - ${ev.security.asx_id} - ev type: ${ev.action.marker}, count: ${ev.units.count}")
        accum(open.updated(ev.security, SecurityTrace(ev)), done, rest)
    }

    val done = prev.mapValues(traces => traces.filter(_.resolved))
    val open = prev.mapValues(traces => traces.find(!_.resolved)).filter(_._2.isDefined).mapValues(_.get)
    accum(open, done, entries)
  }

}
