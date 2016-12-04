package org.shadowlands.tradetracker

import java.io.Writer
import java.time.Month

import org.shadowlands.tradetracker.model.{Money, Traces}

package object reporting {

  def dump(traces: Traces, writer: Writer = System.console.writer): Unit = {
    writer.write(s"\n\nDump of traces:\n\n")
    val sorted = traces.values.flatten.toSeq.sortBy(_.start.toEpochDay)
    sorted.foreach { trace =>
      writer.write(s"Security: ${trace.security} - starting: ${trace.start}, ending: ${trace.end}" +
        s", currently: ${if (trace.resolved) "resolved" else s"${trace.current.count} outstanding"}" +
        s", entries: ${trace.events.size}, net outcome (to-date): ${trace.net_outcome}\n")
    }
    val tot_out = traces.flatMap{ case (code, traces) => traces.filter(_.resolved) }.map(_.net_outcome)
      .foldLeft(Money())((sum, next) => sum + next)
    writer.write(s"\n\nDump complete - total traces: ${sorted.size}, total outcome of resolved traces: $tot_out.\n\n")
  }

  def finYear(yr: Int, mth: Month) = if (mth.getValue > 6) yr else yr-1

  def monthlyTotals(traces: Traces, writer: Writer = System.console.writer): Unit = {
    writer.write(s"\n\nMonthly outcomes:\n\n")
    val sorted = traces.values.flatten.toSeq.sortBy(_.start.toEpochDay)
    val monthly_sets = sorted.filter(_.resolved).groupBy(trace => (trace.end.getYear, trace.end.getMonth))
    val monthly_totals = monthly_sets.mapValues(_.foldLeft(Money())((sum, next) => sum + next.net_outcome))
    monthly_totals.foreach { case ((year, month), total) =>
      writer.write(s"Total for $month, $year: $total\n")
    }
    val fin_year_totals = monthly_totals.toStream.map{ case ((yr,mth), total) => finYear(yr,mth) -> total }
      .groupBy{ case (yr, tot) => yr }.mapValues(_.foldLeft(Money())((sum, next) => sum + next._2))
    fin_year_totals.foreach { case (year, total) =>
      writer.write(s"Total for financial year $year-${year+1}: $total\n")
    }
    val tot_out = traces.flatMap{ case (code, traces) => traces.filter(_.resolved) }.map(_.net_outcome)
      .foldLeft(Money())((sum, next) => sum + next)
    writer.write(s"\n\nTotal of outcomes: $tot_out.\n\n")
  }
}
