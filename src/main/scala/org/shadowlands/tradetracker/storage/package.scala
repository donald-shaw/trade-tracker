package org.shadowlands.tradetracker

import java.io.{PrintWriter, Writer}
import java.nio.file.Path
import scala.io.Source
import spray.json._

import org.shadowlands.tradetracker.model._
import org.shadowlands.tradetracker.model.json.JsonConversions._
import org.shadowlands.tradetracker.model.json.JsonEntities.{JsonTrace, JsonEvent}


package object storage {

  private val divider = "==="

  def storeEvents(events: Seq[Event], store: Writer): Unit = {
    events foreach { event =>
      store.write(s"${toJsonEvent(event).toJson.compactPrint}\n")
    }
  }

  def storeTraces(traces: Traces, store: Writer): Unit = {
    traces foreach { case (sec, traces) =>
      //store.write(s"${sec.asx_id}:\n${traces.map(tr => tr.current).mkString("\n")}\n")
      store.write(s"${sec.asx_id}:\n${traces.map(toJsonTrace).map(_.toJson.compactPrint).mkString("\n")}\n")
    }
  }

  def storeData(events: Seq[Event], traces: Traces, store: Path): Unit = {
    var writer: Writer = null
    try {
      writer = new PrintWriter(store.toFile)
      storeEvents(events, writer)
      writer.write(s"$divider\n")
      storeTraces(traces, writer)
    } catch {
      case ex: Exception => ex.printStackTrace
    } finally {
      if (writer != null) {
        writer.flush()
        writer.close()
      }
    }
  }

  def readEvents(lines: List[String]): Seq[Event] = {
    lines.map(_.parseJson.convertTo[JsonEvent]).map(fromJsonEvent)
  }

  def readTraces(lines: List[String], debug: Boolean): Traces = {
    def buildTraces(traces: Traces, security: Option[Security], current: List[SecurityTrace], lines: List[String]): Traces = (security, current, lines) match {
      case (Some(sec), _, line :: rest) if line.isEmpty =>
        if (debug) println(s"Finished trace for security '$sec'")
        buildTraces(traces.updated(sec, current), None, Nil, rest)
      case (_, _, line :: rest) if line.endsWith(":") =>
        if (debug) println(s"Starting a trace for security '${line.dropRight(1)}'")
        buildTraces(traces, Some(Security(line.dropRight(1))), Nil, rest)
      case (Some(sec), _, line :: rest) =>
        val trace = fromJsonTrace(line.parseJson.convertTo[JsonTrace])
        if (debug) println(s"Adding entry for security '$sec': $trace")
        buildTraces(traces, security, trace :: current, rest)
      case (Some(sec), curr, Nil) =>
        if (debug) println(s"Finished processing old traces - finalise current trace and return all read-in traces: current trace: $curr, other traces: ${traces.mkString}")
        traces.updated(sec, curr)
      case (_, _, Nil) =>
        if (debug) println(s"Finished processing old traces - no current trace, so just return other read-in traces: ${traces.mkString}")
        traces
    }

    buildTraces(Map.empty, None, Nil, lines)
  }

  def readData(store: Path, debug: Boolean = false): (Seq[Event], Traces) = {
    var source: Source = null
    try {
      source = Source.fromFile(store.toFile)
      val (events, traces) = source.getLines().toList.span(_ != divider)
      (readEvents(events), readTraces(traces.drop(1), debug)) // Drop the divider
    } catch {
      case ex: Exception =>
        ex.printStackTrace
        (Nil, Map.empty)
    } finally {
      if (source != null) {
        source.close()
      }
    }
  }
}
