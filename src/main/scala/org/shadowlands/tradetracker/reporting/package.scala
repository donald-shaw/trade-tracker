package org.shadowlands.tradetracker

import java.io.Writer

import org.shadowlands.tradetracker.model.Traces

package object reporting {

  def dump(traces: Traces, writer: Writer = System.console.writer): Unit = {
    writer.write(s"\n\nDump of traces:\n\n")
    val sorted = traces.values.flatten.toSeq.sortBy(_.start.toEpochDay)
    sorted.foreach { trace =>
      writer.write(s"Security: ${trace.security} - starting: ${trace.start}, ending: ${trace.end}" +
        s", currently: ${if (trace.resolved) "resolved" else s"${trace.current.count} outstanding"}" +
        s", entries: ${trace.events.size}, net outcome (to-date): ${trace.net_outcome}\n")
    }
    val tot_out = traces.flatMap{ case (code, traces) => traces.filter(_.resolved) }.map(_.net_outcome.amount).sum
    writer.write(s"\n\nDump complete - total traces: ${sorted.size}, total outcome of resolved traces: $tot_out.\n\n")
  }
}
