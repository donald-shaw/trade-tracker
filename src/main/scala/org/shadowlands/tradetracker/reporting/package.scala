package org.shadowlands.tradetracker

import java.io.Writer

import org.shadowlands.tradetracker.model.Traces

package object reporting {

  def dump(traces: Traces, writer: Writer = System.console.writer): Unit = {
    writer.write(s"\n\nDump of traces:\n\n")
    val sorted = traces.values.flatten.toSeq.sortBy(_.start.toEpochDay)
    sorted.foreach { trace =>
      writer.write(s"Security: ${trace.security} - starting: ${trace.start}")
    }
    writer.write(s"\n\nDump complete - total traces: ${sorted.size}.\n\n")
  }
}
