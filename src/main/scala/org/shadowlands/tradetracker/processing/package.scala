package org.shadowlands.tradetracker

import model._

package object processing {

  def accumEvents(entries: List[Event]): Traces = {

    def finish(open: Map[Security, SecurityTrace], done: Traces) = {
      val updates = for {
        unres <- open
        res = done.getOrElse(unres._1, Nil)
      } yield (unres._1 -> (unres._2 :: res))
      done ++ updates
    }

    def accum(open: Map[Security, SecurityTrace], done: Traces, left: List[Event]): Traces = left match {
      case Nil => finish(open, done)
      case ev :: rest if (open.contains(ev.security)) => (open(ev.security) + ev) match {
        case trace if trace.resolved && done.contains(ev.security) =>
          accum(open - ev.security, done.updated(ev.security, trace :: done(ev.security)), rest)
        case trace if trace.resolved =>
          accum(open - ev.security, done.updated(ev.security, trace :: Nil), rest)
        case trace =>
          accum(open.updated(ev.security, trace), done, rest)
      }
      case ev :: rest => accum(open.updated(ev.security, SecurityTrace(ev)), done, rest)
    }

    accum(Map.empty, Map.empty, entries)
  }

}
